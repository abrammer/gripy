import logging

import struct
import numpy as np

from gripy import g2pylib


def shifting(bitlist):
    out = 0
    for bit in bitlist:
        out = (out << 1) | bit
    return out

# def gbit(buff, offset, nbits):
#     print(offset, nbits)
# #     __input = np.packbits(buff) 
# #     print(len(__input))
#     retvals = shifting(buff[offset:offset+nbits])
#     print(retvals)
#     print('****')
#     return retvals

# def gbits(buff, pos, nbits: int, nskip, n_num):
#     retvals = np.empty(n_num, dtype=np.int32)
#     for n in range(0, n_num):
#         retvals[n] = shifting(buff[pos:pos+nbits])
#         pos += (nbits + nskip)
#     return retvals

def gbit(buff, offset, nbits):
    retvals = gbits(buff, offset, int(nbits), 0, 1)[0]
    return retvals

def gbits(buff, pos, nbits: int, nskip, n_num):
    retvals = np.empty(n_num, dtype=np.int32)
    byte_idx = int(np.floor(pos / 8))
    byte_offset = int((pos % 8))
    fmts = {8:'B', 16:'H', 24:'HB', 32:'I', 64:'Q'}
    if (byte_offset == 0) and (nbits%8 == 0):
        fmt = fmts[nbits]
        if nbits == 24:
            logging.info('24 bit values')
            h,b = struct.unpack_from(f">{n_num}{fmt}", buff, byte_idx)
            retvals[:] = (h << 8) + b
        else:
            retvals[:] = struct.unpack_from(f">{n_num}{fmt}", buff, byte_idx)
    else:
        logging.info(f'non byte offset -- {pos}: {nbits}')
        bit_padding = (8-(nbits % 8))
        byte_size = int(nbits + bit_padding) # size of byte to hold data % 8 == 0
        if byte_size == 24:
            byte_size = 32
            bit_padding += 8
        tot_bits = n_num * nbits
        start_pos = byte_offset
        end_pos = start_pos + tot_bits
        byte_count = np.max([1, int(np.ceil(end_pos/8))])
        # print(buff[byte_idx: byte_idx+byte_count])
        membuff = np.frombuffer(buff, dtype='>B', count=byte_count, offset=byte_idx)
        bits = np.unpackbits(membuff)

        output = np.zeros((n_num, byte_size), dtype=np.uint8)
        if n_num == 1:
            output[:, bit_padding:] = bits[start_pos:end_pos]
        else:
            splitted = np.split(bits[start_pos:end_pos], n_num)
            output[:, bit_padding:] = splitted

        unpack_fmt = f">{n_num}{fmts[byte_size]}"
        retvals[:] = struct.unpack_from(unpack_fmt, np.packbits(output))

    return retvals


def comunpack(cpack, lensec, idrsnum, idrstmpl, ndpts):
    #$$$  SUBPROGRAM DOCUMENTATION BLOCK
    #                .      .    .                                       .
    # SUBPROGRAM:    comunpack
    #   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2002-10-29
    #
    # ABSTRACT: This subroutine unpacks a data field that was packed using a
    #   complex packing algorithm as defined in the GRIB2 documention,
    #   using info from the GRIB2 Data Representation Template 5.2 or 5.3.
    #   Supports GRIB2 complex packing templates with or without
    #   spatial differences (i.e. DRTs 5.2 and 5.3).
    #
    # PROGRAM HISTORY LOG:
    # 2002-10-29  Gilbert
    # 2004-12-16  Gilbert  -  Added test ( provided by Arthur Taylor/MDL )
    #                         to verify that group widths and lengths are
    #                         consistent with section length.
    #
    # USAGE:    int comunpack(unsigned char *cpack,g2int lensec,g2int idrsnum,
    #                         g2int *idrstmpl, g2int ndpts,g2float *fld)
    #   INPUT ARGUMENT LIST:
    #     cpack    - pointer to the packed data field.
    #     lensec   - length of section 7 (used for error checking).
    #     idrsnum  - Data Representation Template number 5.N
    #                Must equal 2 or 3.
    #     idrstmpl - pointer to the array of values for Data Representation
    #                Template 5.2 or 5.3
    #     ndpts    - The number of data values to unpack
    #
    #   OUTPUT ARGUMENT LIST:
    #     fld      - Contains the unpacked data values.  fld must be allocated
    #                with at least ndpts*sizeof(g2float) bytes before
    #                calling this routine.
    #
    # REMARKS: None
    #
    # ATTRIBUTES:
    #   LANGUAGE: C
    #   MACHINE:
    #
    #$$$#
    #   g2int   nbitsd=0,isign
    #   g2int  j,iofst,ival1,ival2,minsd,itemp,l,k,n,non=0
    #   g2int  *ifld,*ifldmiss=0
    #   g2int  *gref,*gwidth,*glen
    #   g2int  itype,ngroups,nbitsgref,nbitsgwidth,nbitsglen
    #   g2int  msng1,msng2
    #   g2float ref,bscale,dscale,rmiss1,rmiss2
    #   g2int totBit, totLen

    # print(f"IDRSTMPL: {idrstmpl}")
    ref = g2pylib.rdieee(idrstmpl[0])
    # print("SAGTref: {ref}")
    bscale = 2.0**idrstmpl[1]
    dscale = 10.0**-idrstmpl[2]
    nbitsgref = idrstmpl[3]
    itype = idrstmpl[4]
    ngroups = idrstmpl[9]
    nbitsgwidth = idrstmpl[11]
    nbitsglen = idrstmpl[15]
    if (idrsnum == 3):
        nbitsd=idrstmpl[17]*8
    # print(f"nbitsd: {nbitsd}")
      #   Constant field

    if (ngroups == 0):
        fld = np.empty(ndpts)
        fld[:] = ref
        return fld
    fldmiss = np.zeros(ndpts)


    iofst=0
    ifld = np.empty(ndpts, dtype=np.int)
    # print(f"ALLOC ifld: {ndpts},")
    gref = np.empty(ngroups, dtype=np.int)
    # print(f"ALLOC gref: {ngroups}, ")
    gwidth=np.empty(ngroups, dtype=np.int)
    # print(f"ALLOC gwidth: {ngroups}, {gwidth}")
    #
    #  Get missing values, if supplied
    #
    rmiss1=0
    rmiss2=0
    if ( idrstmpl[6] == 1 ):
        if (itype == 0):
            rmiss1 = g2pylib.rdieee(idrstmpl[7])
        else:
            rmiss1= idrstmpl[7]

    if ( idrstmpl[6] == 2 ):
        if (itype == 0):
            rmiss1= g2pylib.rdieee(idrstmpl[7])
            rmiss2= g2pylib.rdieee(idrstmpl[8])
        else:
            rmiss1=idrstmpl[7]
            rmiss2=idrstmpl[8]

    # print(f"RMISSs:{rmiss1},{rmiss2},{ref}")
    #
    #  Extract Spatial differencing values, if using DRS Template 5.3
    #
    logging.info('Extract offset values')
    if (idrsnum == 3):
        if (nbitsd != 0):
            ival1 = gbit(cpack,iofst,nbitsd)
            iofst += nbitsd
            if (idrstmpl[16] == 2):
                ival2 = gbit(cpack,iofst,nbitsd)
                iofst += nbitsd

            isign =  gbit(cpack, iofst, 1)
            iofst += 1
            minsd = gbit(cpack,iofst,nbitsd-1)
            if (isign == 1):
                minsd=-minsd

            iofst += nbitsd-1
        else:
            ival1=0
            ival2=0
            minsd=0
    # print(ival1, ival2, minsd)
    # print(f"SDu {ival1},{ival2},{minsd},{nbitsd}")

    #
    #  Extract Each Group's reference value
    #
    # print(f"SAG1: {nbitsgref},{ngroups},{iofst}")
    logging.info('Extract group reference values')
    if (nbitsgref != 0):
        gref[:] = gbits(cpack,iofst,nbitsgref,0,ngroups)
        itemp=nbitsgref*ngroups
        #np.savetxt('p3_gref', cpack[iofst:iofst+itemp], '%1i')
        iofst=iofst+itemp
        if (itemp%8 != 0):
            iofst=iofst+(8-(itemp%8))

    else:
        gref[:]=0
    #
    #  Extract Each Group's bit width
    #
    # print(f"SAG2:{nbitsgwidth},{ngroups},{iofst},{idrstmpl[10]}")
    logging.info('Extract groups bit width values')
    if (nbitsgwidth != 0):
        gwidth[:] = gbits(cpack, iofst, nbitsgwidth, 0, ngroups)
        itemp=nbitsgwidth*ngroups
        iofst=iofst+itemp
        if (itemp%8 != 0):
            iofst=iofst+(8-(itemp%8))

    else:
        gwidth[:]=0

    gwidth[:] += idrstmpl[10]

    #
    #  Extract Each Group's length (number of values in each group)
    #
    logging.info('Extract groups length ')
    glen = np.empty(ngroups, dtype=np.int)
    # print(f"ALLOC glen: {ngroups},")
    # print(f"SAG3: {nbitsglen},{ngroups},{iofst},{idrstmpl[13]},{idrstmpl[12]}")
    if (nbitsglen != 0):
        glen = gbits(cpack, iofst, nbitsglen, 0, ngroups)
        itemp=nbitsglen*ngroups
        iofst=iofst+itemp
        if (itemp%8 != 0):
            iofst=iofst+(8-(itemp%8))

    else:
        glen[:]=0

    glen[:] = (glen[:]*idrstmpl[13])+idrstmpl[12]
    glen[-1] = idrstmpl[14]
    # print(glen[:8])
    #
    #  Test to see if the group widths and lengths are consistent with number of
    #  values, and length of section 7.
    #

    totBit = np.sum(gwidth*glen)
    totLen = np.sum(glen)
    # print(f'{gwidth}, {glen}')
    
    if (totLen != ndpts):
        print('totLen != ndpts')
        print(f'{totLen} != {ndpts}')
        return 1

    if (totBit / 8. > lensec):
        print('totBit / 8. > lensec')
        print('{totBit} / 8. > {lensec}')
        return 1

    #
    #  For each group, unpack data values
    #
    n=0
    non=0


    logging.info('Read Values')
    if ( idrstmpl[6] == 0 ):        # no missing values
        n=0
        for j in range(0, ngroups):
            ifld[n:n+glen[j]] = gref[j]
            if (gwidth[j] != 0):
                ifld[n:n+glen[j]] += gbits(cpack,iofst,gwidth[j],0,glen[j])
            n += glen[j]
            iofst += (gwidth[j] * glen[j])


    # elif ( idrstmpl[6]==1 or idrstmpl[6]==2 ):
    #     # missing values included
    #     ifldmiss=np.zeros(ndpts)
    #     #printf("ALLOC ifldmiss: %d %x\n",(int)ndpts,ifldmiss)
    #     n=0
    #     non=0
    #     for j in range(0, ngroups):
    #        #printf(" SAGNGP %d %d %d %d\n",j,gwidth[j],glen[j],gref[j])
    #         if (gwidth[j] != 0):
    #             msng1=(2.0**gwidth[j])-1
    #             msng2=msng1-1
    #             gbits(cpack,ifld+n,iofst,gwidth[j],0,glen[j])
    #             iofst=iofst+(gwidth[j]*glen[j])
    #             for k in range(0, glen[j]):
    #                 if (ifld[n] == msng1):
    #                     ifldmiss[n]=1
    #                 #ifld[n]=0

    #                 elif (idrstmpl[6]==2 and ifld[n]==msng2):
    #                     ifldmiss[n]=2
    #               #ifld[n]=0

    #                 else:
    #                     ifldmiss[n]=0
    #                     ifld[non++]=ifld[n]+gref[j]

    #                 n++

    #         else:
    #             msng1=(g2int)int_power(2.0,nbitsgref)-1
    #             msng2=msng1-1
    #             if (gref[j] == msng1):
    #                 for (l=n;l<n+glen[j];l++):
    #                     ifldmiss[l]=1

    #             elif (idrstmpl[6]==2  gref[j]==msng2):
    #                 for (l=n;l<n+glen[j];l++):
    #                     ifldmiss[l]=2

    #             else:
    #                 for (l=n;l<n+glen[j];l++):
    #                     ifldmiss[l]=0
    #                 for (l=non;l<non+glen[j];l++):
    #                     ifld[l]=gref[j]
    #                 non += glen[j]
    #             n=n+glen[j]
    # #
    #  If using spatial differences, add overall min value, and
    #  sum up recursively
    #
    #printf("SAGod: %ld %ld\n",idrsnum,idrstmpl[16])
    logging.info(f'Start Spatial Differencing {idrsnum},{idrstmpl[16]} ')
    if (idrsnum == 3):         # spatial differencing
        if (idrstmpl[16] == 1):      # first order
            ifld[0]=ival1
            if ( idrstmpl[6] == 0 ):
                itemp=ndpts        # no missing values
            else:
                itemp=non
            for n in range(1, itemp):
                ifld[n]=ifld[n]+minsd
                ifld[n]=ifld[n]+ifld[n-1]

        elif (idrstmpl[16] == 2):    # second order
            ifld[0]=ival1
            ifld[1]=ival2
            if ( idrstmpl[6] == 0 ):
                itemp=ndpts        # no missing values
            else:
                itemp=non
            ifld[2:] += minsd

            logging.info(f'Loop through array')

            # ifld = np.cumsum(np.append(ifld[0:1], np.cumsum(np.append(ifld[1]-ifld[0], ifld[2:]))))
        
            # 2 pass, undo 2nd order diff, then undo 1st order diff
            # use cumsum in numpy for speed
            ifld[1] = ifld[1]-ifld[0]
            ifld[1:] = np.cumsum(ifld[1:])  # Good  first value, then first order diff
            ifld[:] = np.cumsum(ifld)  # undo first order to get orignal values
            # original c loop:
            # for n in range(2, itemp):
                # ifld[n] = ifld[n] + (2*ifld[n-1])-ifld[n-2]

    logging.info(f'Done Loop through array')
    #
    #  Scale data back to original form
    #
      #printf("SAGT: %f %f %f\n",ref,bscale,dscale)
    if ( idrstmpl[6] == 0 ):       # no missing values
        # for n in range(0, ndpts):
        logging.info('scale data back to real values')
        fld=((ifld*bscale)+ref)*dscale

    elif ( idrstmpl[6]==1 or idrstmpl[6]==2 ):
         # missing values included
        non=0
        for n in range(0, ndpts):
            if ( fldmiss[n] == 0 ):
                non += 1
                fld[n]=((ifld[non]*bscale)+ref)*dscale
                #printf(" SAG %d %f %d %f %f %f\n",n,fld[n],ifld[non-1],bscale,ref,dscale)

            elif ( fldmiss[n] == 1 ):
                fld[n]=rmiss1
            elif ( fldmiss[n] == 2 ):
                fld[n]=rmiss2


    return fld

