            PAGE 0                          ; suppress page headings in ASW listing file

            cpu 6502
            
            include functions.inc         

; Microsoft BASIC for 6502 (OSI VERSION) and 6502 SYSMON (system monitor)
;
; serial I/O at 115200 bps, N-8-1
;
; ==================================================================================
; MODIFIED FROM THE ORIGINAL FILES AT http://www.pagetable.com/?p=46
; MERGED INTO ONE FILE AND MACROS AND CONDITIONAL STATEMENTS REMOVED
; BY G. SEARLE 2013
;
; I/O and dummy load/saves added to the end of this code
;
; This then assembles to the OSI version with the following
; minor changes to the original BASIC code:
; 1. Control-C call changed
; 2. Load/save calls changed
; 3. RAM start set to $0200 instead of $0300
; 4. ROM start set to $C000
; 5. Second letter of error messages back to ASCII value (instead of $80+val)
; ==================================================================================
;
; Converted to Macro Assembler AS V1.42 http://john.ccac.rwth-aachen.de:8000/as/ syntax - Jim Loos 6/20/2024
;
;
; Extract of original header comments follows:
;
; (first revision of this distribution, 20 Oct 2008, Michael Steil www.pagetable.com)
;
;
; Name                 Release   MS Version    ROM   9digit  INPUTBUFFER   extensions   
;---------------------------------------------------------------------------------------------------
; OSI BASIC             1977     1.0 REV 3.2    Y      N          ZP            -        
;
; Credits:
; * main work by Michael Steil
; * function names and all uppercase comments taken from Bob Sander-Cederlof's excellent AppleSoft II disassembly:
;   http://www.txbobsc.com/scsc/scdocumentor/
; * Applesoft lite by Tom Greene http://cowgod.org/replica1/applesoft/ helped a lot, too.
; * Thanks to Joe Zbicak for help with Intellision Keyboard BASIC
; * This work is dedicated to the memory of my dear hacking pal Michael "acidity" Kollmann.

; zero page
ZP_START1            equ $00
ZP_START2            equ $0D
ZP_START3            equ $5B
ZP_START4            equ $65

;extra ZP variables
USR                  equ $000A

; constants
STACK_TOP            equ $FC
SPACE_FOR_GOSUB      equ $33
NULL_MAX		         equ $0A
WIDTH			         equ 72
WIDTH2			      equ 56

; memory layout
RAMSTART2		      equ $0200
BYTES_FP		         equ 4
BYTES_PER_ELEMENT    equ BYTES_FP
BYTES_PER_VARIABLE   equ BYTES_FP+2
MANTISSA_BYTES	      equ BYTES_FP-1
BYTES_PER_FRAME      equ 2*BYTES_FP+8
FOR_STACK1		      equ 2*BYTES_FP+5
FOR_STACK2		      equ BYTES_FP+4
MAX_EXPON = 10
STACK                equ $0100

INPUTBUFFERX         equ INPUTBUFFER & $FF00

CR                   equ 13
LF                   equ 10

CRLF_1               equ CR
CRLF_2               equ LF

                     org $0000
                     org ZP_START1

GORESTART:           ds 3
GOSTROUT:            ds 3
GOAYINT:             ds 2
GOGIVEAYF:           ds 2

                     org ZP_START2
                     
Z15:                 ds 1
POSX:                ds 1
Z17:                 ds 1
Z18:                 ds 1
LINNUM:
TXPSV:               ds 2
INPUTBUFFER:

                     org ZP_START3
                  
CHARAC:              ds 1
ENDCHR:              ds 1
EOLPNTR:             ds 1
DIMFLG:              ds 1
VALTYP:              ds 1
DATAFLG:             ds 1
SUBFLG:              ds 1
INPUTFLG:            ds 1
CPRMASK:             ds 1
Z14:                 ds 1

                     org ZP_START4
                     
TEMPPT:              ds 1
LASTPT:              ds 2
TEMPST:              ds 9
INDEX:               ds 2
DEST:                ds 2
RESULT:              ds BYTES_FP
RESULT_LAST          equ RESULT + BYTES_FP-1
TXTTAB:              ds 2
VARTAB:         	   ds 2
ARYTAB:         	   ds 2
STREND:         	   ds 2
FRETOP:         	   ds 2
FRESPC:         	   ds 2
MEMSIZ:         	   ds 2
CURLIN:         	   ds 2
OLDLIN:         	   ds 2
OLDTEXT:        	   ds 2
Z8C:            	   ds 2
DATPTR:         	   ds 2
INPTR:          	   ds 2
VARNAM:         	   ds 2
VARPNT:         	   ds 2
FORPNT:         	   ds 2
LASTOP:         	   ds 2
CPRTYP:         	   ds 1
FNCNAM:
TEMP3:          	   ds 2
DSCPTR:         	   ds 2
DSCLEN:         	   ds 2
JMPADRS	            equ DSCLEN + 1
Z52:            	   ds 1
ARGEXTENSION:
TEMP1:          	   ds 1
HIGHDS:         	   ds 2
HIGHTR:         	   ds 2
INDX:
TMPEXP:
TEMP2:          	   ds 1
EXPON:          	   ds 1
LOWTR:
LOWTRX:         	   ds 1
EXPSGN:         	   ds 1
FAC:            	   ds BYTES_FP
FAC_LAST = FAC + BYTES_FP-1
FACSIGN:        	   ds 1
SERLEN:         	   ds 1
SHIFTSIGNEXT:   	   ds 1
ARG:            	   ds BYTES_FP
ARG_LAST = ARG + BYTES_FP-1
ARGSIGN:        	   ds 1
STRNG1:     	      ds 2
SGNCPR = STRNG1
FACEXTENSION = STRNG1+1
STRNG2:         	   ds 2
CHRGET:
TXTPTR               equ lo(GENERIC_TXTPTR-GENERIC_CHRGET + CHRGET)
CHRGOT               equ lo(GENERIC_CHRGOT-GENERIC_CHRGET + CHRGET)
CHRGOT2              equ lo(GENERIC_CHRGOT2-GENERIC_CHRGET + CHRGET)
RNDSEED              equ lo(GENERIC_RNDSEED-GENERIC_CHRGET + CHRGET)

                     org $C000
TOKEN_ADDRESS_TABLE:
                     dw END-1
                     dw FOR-1
                     dw NEXT-1
                     dw DATA-1
                     dw INPUT-1
                     dw DIM-1
                     dw READ-1
                     dw LET-1
TOKEN_GOTO           equ $80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw GOTO-1
                     dw RUN-1
                     dw IF-1
                     dw RESTORE-1
TOKEN_GOSUB          equ $80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw GOSUB-1
                     dw POP-1
TOKEN_REM            equ $80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw REM-1
                     dw STOP-1
                     dw ON-1
                     dw NULL-1
                     dw WAIT-1
                     dw LOAD-1
                     dw SAVE-1
                     dw DEF-1
                     dw POKE-1
TOKEN_PRINT          equ $80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw PRINT-1
                     dw CONT-1
                     dw LIST-1
                     dw CLEAR-1
                     dw NEW-1
                    
TOKEN_TAB            equ $00+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_TO             equ $01+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_FN             equ $02+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_SPC            equ $03+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_THEN           equ $04+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_NOT            equ $05+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_STEP           equ $06+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_PLUS           equ $07+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_MINUS          equ $08+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_GREATER        equ $0E+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_EQUAL          equ $0F+$80+(*-TOKEN_ADDRESS_TABLE)/2
NUM_TOKENS           equ (*-TOKEN_ADDRESS_TABLE)/2

UNFNC:
TOKEN_SGN equ $11+$80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw SGN
                     dw INT
                     dw ABS
                     dw USR
                     dw FRE
                     dw POS
                     dw SQR
                     dw RND
                     dw LOG
                     dw EXP
                     dw COS
                     dw SIN
                     dw TAN
                     dw ATN
                     dw PEEK
                     dw LEN
                     dw STR
                     dw VAL
                     dw ASC
                     dw CHRSTR
TOKEN_LEFTSTR equ $11+$80+(*-TOKEN_ADDRESS_TABLE)/2
                     dw LEFTSTR
                     dw RIGHTSTR
                     dw MIDSTR

MATHTBL:
                     db   $79
                     dw   FADDT-1
                     db   $79
                     dw   FSUBT-1
                     db   $7B
                     dw   FMULTT-1
                     db   $7B
                     dw   FDIVT-1
                     db   $7F
                     dw   FPWRT-1
                     db   $50
                     dw   TAND-1
                     db   $46
                     dw   OR-1
                     db   $7D
                     dw   NEGOP-1
                     db   $5A
                     dw   EQUOP-1
                     db   $64
                     dw   RELOPS-1
		
TOKEN_NAME_TABLE:
                     db "EN", $80+'D'
                     db "FO", $80+'R'
                     db "NEX", $80+'T'
                     db "DAT", $80+'A'
                     db "INPU", $80+'T'
                     db "DI", $80+'M'
                     db "REA", $80+'D'
                     db "LE", $80+'T'
                     db "GOT", $80+'O'
                     db "RU", $80+'N'
                     db "I", $80+'F'
                     db "RESTOR", $80+'E'
                     db "GOSU", $80+'B'
                     db "RETUR", $80+'N'
                     db "RE", $80+'M'
                     db "STO", $80+'P'
                     db "O", $80+'N'
                     db "NUL", $80+'L'
                     db "WAI", $80+'T'
                     db "LOA", $80+'D'
                     db "SAV", $80+'E'
                     db "DE", $80+'F'
                     db "POK", $80+'E'
                     db "PRIN", $80+'T'
                     db "CON", $80+'T'
                     db "LIS", $80+'T'
                     db "CLEA", $80+'R'
                     db "NE", $80+'W'
                     db "TAB", $80+'('
                     db "T", $80+'O'
                     db "F", $80+'N'
                     db "SPC", $80+'('
                     db "THE", $80+'N'
                     db "NO", $80+'T'
                     db "STE", $80+'P'
                     db "", $80+'+'
                     db "", $80+'-'
                     db "", $80+'*'
                     db "", $80+'/'
                     db "", $80+'^'
                     db "AN", $80+'D'
                     db "O", $80+'R'
                     db "", $80+'>'
                     db "", $80+'='
                     db "", $80+'<'
                     db "SG", $80+'N'
                     db "IN", $80+'T'
                     db "AB", $80+'S'
                     db "US", $80+'R'
                     db "FR", $80+'E'
                     db "PO", $80+'S'
                     db "SQ", $80+'R'
                     db "RN", $80+'D'
                     db "LO", $80+'G'
                     db "EX", $80+'P'
                     db "CO", $80+'S'
                     db "SI", $80+'N'
                     db "TA", $80+'N'
                     db "AT", $80+'N'
                     db "PEE", $80+'K'
                     db "LE", $80+'N'
                     db "STR", $80+'$'
                     db "VA", $80+'L'
                     db "AS", $80+'C'
                     db "CHR", $80+'$'
                     db "LEFT", $80+'$'
                     db "RIGHT", $80+'$'
                     db "MID", $80+'$'
                     db   0
		
ERROR_MESSAGES:
ERR_NOFOR            equ lo(*-ERROR_MESSAGES)
                     db "NF"
ERR_SYNTAX           equ lo(*-ERROR_MESSAGES)
                     db "SN"
ERR_NOGOSUB          equ lo(*-ERROR_MESSAGES)
                     db "RG"
ERR_NODATA           equ lo(*-ERROR_MESSAGES)
                     db "OD"
ERR_ILLQTY           equ lo(*-ERROR_MESSAGES)
                     db "FC"
ERR_OVERFLOW         equ lo(*-ERROR_MESSAGES)
                     db "OV"
ERR_MEMFULL          equ lo(*-ERROR_MESSAGES)
                     db "OM"
ERR_UNDEFSTAT        equ lo(*-ERROR_MESSAGES)
                     db "US"
ERR_BADSUBS          equ lo(*-ERROR_MESSAGES)
                     db "BS"
ERR_REDIMD           equ lo(*-ERROR_MESSAGES)
                     db "DD"
ERR_ZERODIV          equ lo(*-ERROR_MESSAGES)
                     db "/0"
ERR_ILLDIR           equ lo(*-ERROR_MESSAGES)
                     db "ID"
ERR_BADTYPE          equ lo(*-ERROR_MESSAGES)
                     db "TM"
ERR_STRLONG          equ lo(*-ERROR_MESSAGES)
                     db "LS"
ERR_FRMCPX           equ lo(*-ERROR_MESSAGES)
                     db "ST"
ERR_CANTCONT         equ lo(*-ERROR_MESSAGES)
                     db "CN"
ERR_UNDEFFN          equ lo(*-ERROR_MESSAGES)
                     db "UF"
		
; global messages: "error", "in", "ready", "break"
QT_ERROR:
                     db   " ERROR"
                     db   0
QT_IN:
                     db   " IN "
                     db   $00
QT_OK:
                     db   CR,LF,"OK",CR,LF
                     db	0
QT_BREAK:
                     db CR,LF,"BREAK"
                     db   0
		
; generic stack and memory management code
; this code is identical across all versions of
; BASIC
; ----------------------------------------------------------------------------
; CALLED BY "NEXT" AND "FOR" TO SCAN THROUGH
; THE STACK FOR A FRAME WITH THE SAME VARIABLE.
;
; (FORPNT) = ADDRESS OF VARIABLE IF "FOR" OR "NEXT"
; 	= $XXFF IF CALLED FROM "RETURN"
; 	<<< BUG: SHOULD BE $FFXX >>>
;
;	RETURNS .NE. IF VARIABLE NOT FOUND,
;	(X) = STACK PNTR AFTER SKIPPING ALL FRAMES
;
;	.EQ. IF FOUND
;	(X) = STACK PNTR OF FRAME FOUND
; ----------------------------------------------------------------------------
GTFORPNT:
        tsx
        inx
        inx
        inx
        inx
L2279:
        lda     STACK+1,x
        cmp     #$81
        bne     L22A1
        lda     FORPNT+1
        bne     L228E
        lda     STACK+2,x
        sta     FORPNT
        lda     STACK+3,x
        sta     FORPNT+1
L228E:
        cmp     STACK+3,x
        bne     L229A
        lda     FORPNT
        cmp     STACK+2,x
        beq     L22A1
L229A:
        txa
        clc
        adc     #BYTES_PER_FRAME
        tax
        bne     L2279
L22A1:
        rts
; ----------------------------------------------------------------------------
; MOVE BLOCK OF MEMORY UP
;
; ON ENTRY:
;	(Y,A) = (HIGHDS) = DESTINATION END+1
;	(LOWTR) = LOWEST ADDRESS OF SOURCE
;	(HIGHTR) = HIGHEST SOURCE ADDRESS+1
; ----------------------------------------------------------------------------
BLTU:
        jsr     REASON
        sta     STREND
        sty     STREND+1
BLTU2:
        sec
        lda     HIGHTR
        sbc     LOWTR
        sta     INDEX
        tay
        lda     HIGHTR+1
        sbc     LOWTR+1
        tax
        inx
        tya
        beq     L22DD
        lda     HIGHTR
        sec
        sbc     INDEX
        sta     HIGHTR
        bcs     L22C6
        dec     HIGHTR+1
        sec
L22C6:
        lda     HIGHDS
        sbc     INDEX
        sta     HIGHDS
        bcs     L22D6
        dec     HIGHDS+1
        bcc     L22D6
L22D2:
        lda     (HIGHTR),y
        sta     (HIGHDS),y
L22D6:
        dey
        bne     L22D2
        lda     (HIGHTR),y
        sta     (HIGHDS),y
L22DD:
        dec     HIGHTR+1
        dec     HIGHDS+1
        dex
        bne     L22D6
        rts
; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM LEFT ON STACK
; FOR "FOR", "GOSUB", OR EXPRESSION EVALUATION
; ----------------------------------------------------------------------------
CHKMEM:
        asl     a
        adc     #SPACE_FOR_GOSUB
        bcs     MEMERR
        sta     INDEX
        tsx
        cpx     INDEX
        bcc     MEMERR
        rts
; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM BETWEEN ARRAYS AND STRINGS
; (Y,A) = ADDR ARRAYS NEED TO GROW TO
; ----------------------------------------------------------------------------
REASON:
        cpy     FRETOP+1
        bcc     L231E
        bne     L22FC
        cmp     FRETOP
        bcc     L231E
L22FC:
        pha
        ldx     #FAC-TEMP1-1
        tya
L2300:
        pha
        lda     TEMP1,x
        dex
        bpl     L2300
        jsr     GARBAG
        ldx     #TEMP1-FAC+1
L230B:
        pla
        sta     FAC,x
        inx
        bmi     L230B
        pla
        tay
        pla
        cpy     FRETOP+1
        bcc     L231E
        bne     MEMERR
        cmp     FRETOP
        bcs     MEMERR
L231E:
        rts
MEMERR:
        ldx     #ERR_MEMFULL
; ----------------------------------------------------------------------------
; HANDLE AN ERROR
;
; (X)=OFFSET IN ERROR MESSAGE TABLE
; (ERRFLG) > 128 IF "ON ERR" TURNED ON
; (CURLIN+1) = $FF IF IN DIRECT MODE
; ----------------------------------------------------------------------------
ERROR:
        lsr     Z14
        jsr     CRDO
        jsr     OUTQUES
        lda     ERROR_MESSAGES,x
        jsr     OUTDO
        lda     ERROR_MESSAGES+1,x
        jsr     OUTDO
        jsr     STKINI
        lda     #lo(QT_ERROR)
        ldy     #hi(QT_ERROR)
; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; PRINT CURRENT LINE # UNLESS IN DIRECT MODE
; FALL INTO WARM RESTART
; ----------------------------------------------------------------------------
PRINT_ERROR_LINNUM:
        jsr     STROUT
        ldy     CURLIN+1
        iny
        beq     RESTART
        jsr     INPRT
        
; ----------------------------------------------------------------------------
; WARM RESTART ENTRY
; ----------------------------------------------------------------------------
RESTART:
        lsr     Z14
        lda     #lo(QT_OK)
        ldy     #hi(QT_OK)
        jsr     GOSTROUT
L2351:
        jsr     INLIN
        stx     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGET
; bug in pre-1.1: CHRGET sets Z on '\0'
; and ':' - a line starting with ':' in
; direct mode gets ignored
        beq     L2351
        ldx     #$FF
        stx     CURLIN+1
        bcc     NUMBERED_LINE
        jsr     PARSE_INPUT_LINE
        jmp     NEWSTT2
        
; ----------------------------------------------------------------------------
; HANDLE NUMBERED LINE
; ----------------------------------------------------------------------------
NUMBERED_LINE:
        jsr     LINGET
        jsr     PARSE_INPUT_LINE
        sty     EOLPNTR
        jsr     FNDLIN
        bcc     PUT_NEW_LINE
        ldy     #$01
        lda     (LOWTR),y
        sta     INDEX+1
        lda     VARTAB
        sta     INDEX
        lda     LOWTR+1
        sta     DEST+1
        lda     LOWTR
        dey
        sbc     (LOWTR),y
        clc
        adc     VARTAB
        sta     VARTAB
        sta     DEST
        lda     VARTAB+1
        adc     #$FF
        sta     VARTAB+1
        sbc     LOWTR+1
        tax
        sec
        lda     LOWTR
        sbc     VARTAB
        tay
        bcs     L23A5
        inx
        dec     DEST+1
L23A5:
        clc
        adc     INDEX
        bcc     L23AD
        dec     INDEX+1
        clc
L23AD:
        lda     (INDEX),y
        sta     (DEST),y
        iny
        bne     L23AD
        inc     INDEX+1
        inc     DEST+1
        dex
        bne     L23AD
; ----------------------------------------------------------------------------
PUT_NEW_LINE:
        lda     INPUTBUFFER
        beq     FIX_LINKS
        lda     MEMSIZ
        ldy     MEMSIZ+1
        sta     FRETOP
        sty     FRETOP+1
        lda     VARTAB
        sta     HIGHTR
        adc     EOLPNTR
        sta     HIGHDS
        ldy     VARTAB+1
        sty     HIGHTR+1
        bcc     L23D6
        iny
L23D6:
        sty     HIGHDS+1
        jsr     BLTU
        lda     STREND
        ldy     STREND+1
        sta     VARTAB
        sty     VARTAB+1
        ldy     EOLPNTR
        dey
; ---COPY LINE INTO PROGRAM-------
L23E6:
        lda     INPUTBUFFER-4,y
        sta     (LOWTR),y
        dey
        bpl     L23E6
        
; ----------------------------------------------------------------------------
; CLEAR ALL VARIABLES
; RE-ESTABLISH ALL FORWARD LINKS
; ----------------------------------------------------------------------------
FIX_LINKS:
        jsr     SETPTRS
        lda     TXTTAB
        ldy     TXTTAB+1
        sta     INDEX
        sty     INDEX+1
        clc
L23FA:
        ldy     #$01
        lda     (INDEX),y
        bne     *+5
        jmp     L2351

        ldy     #$04
L2405:
        iny
        lda     (INDEX),y
        bne     L2405
        iny
        tya
        adc     INDEX
        tax
        ldy     #$00
        sta     (INDEX),y
        lda     INDEX+1
        adc     #$00
        iny
        sta     (INDEX),y
        stx     INDEX
        sta     INDEX+1
        bcc     L23FA	; always
; ----------------------------------------------------------------------------
L2420:
        jsr     OUTDO
        dex
        bpl     INLIN2
L2423:
        jsr     OUTDO
        jsr     CRDO
        
; ----------------------------------------------------------------------------
; READ A LINE, AND STRIP OFF SIGN BITS
; ----------------------------------------------------------------------------
INLIN:
        ldx     #$00
INLIN2:
        jsr     GETLN
        cmp     #$07
        beq     L2443
        cmp     #$0D
        beq     L2453
        cmp     #$08 ; BACKSPACE
        beq     L2420
        cmp     #$20
        bcc     INLIN2
        cmp     #$7F                    ; changed from 'cmp #$7D' jsl 12/28/2021
        bcs     INLIN2
        cmp     #$40 ; @
        beq     L2423
L2443:
        cpx     #$47
        bcs     L244C
        sta     INPUTBUFFER,x
        inx
        db   $2C
L244C:
        lda     #$07 ; BEL
        jsr     OUTDO
        bne     INLIN2
L2453:
        jmp     L29B9
GETLN:
        jsr     MONRDKEY
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        and     #$7F
RDKEY:
        cmp     #$0F
        bne     L2465
        pha
        lda     Z14
        eor     #$FF
        sta     Z14
        pla
L2465:
        rts
        
; ----------------------------------------------------------------------------
; TOKENIZE THE INPUT LINE
; ----------------------------------------------------------------------------
PARSE_INPUT_LINE:
        ldx     TXTPTR
        ldy     #$04
        sty     DATAFLG
L246C:
        lda     INPUTBUFFERX,x
        cmp     #$20
        beq     L24AC
        sta     ENDCHR
        cmp     #$22
        beq     L24D0
        bit     DATAFLG
        bvs     L24AC
        cmp     #$3F
        bne     L2484
        lda     #TOKEN_PRINT
        bne     L24AC
L2484:
        cmp     #$30
        bcc     L248C
        cmp     #$3C
        bcc     L24AC
; ----------------------------------------------------------------------------
; SEARCH TOKEN NAME TABLE FOR MATCH STARTING
; WITH CURRENT CHAR FROM INPUT LINE
; ----------------------------------------------------------------------------
L248C:
        sty     STRNG2
        ldy     #$00
        sty     EOLPNTR
        dey
        stx     TXTPTR
        dex
L2496:
        iny
L2497:
        inx
L2498:
        lda     INPUTBUFFERX,x
        cmp     #$20
        beq     L2497
        sec
        sbc     TOKEN_NAME_TABLE,y
        beq     L2496
        cmp     #$80
        bne     L24D7
        ora     EOLPNTR
; ----------------------------------------------------------------------------
; STORE CHARACTER OR TOKEN IN OUTPUT LINE
; ----------------------------------------------------------------------------
L24AA:
        ldy     STRNG2
L24AC:
        inx
        iny
        sta     INPUTBUFFER-5,y
        lda     INPUTBUFFER-5,y
        beq     L24EA
        sec
        sbc     #$3A
        beq     L24BF
        cmp     #$49
        bne     L24C1
L24BF:
        sta     DATAFLG
L24C1:
        sec
        sbc     #TOKEN_REM-':'
        bne     L246C
        sta     ENDCHR
; ----------------------------------------------------------------------------
; HANDLE LITERAL (BETWEEN QUOTES) OR REMARK,
; BY COPYING CHARS UP TO ENDCHR.
; ----------------------------------------------------------------------------
L24C8:
        lda     INPUTBUFFERX,x
        beq     L24AC
        cmp     ENDCHR
        beq     L24AC
L24D0:
        iny
        sta     INPUTBUFFER-5,y
        inx
        bne     L24C8
; ----------------------------------------------------------------------------
; ADVANCE POINTER TO NEXT TOKEN NAME
; ----------------------------------------------------------------------------
L24D7:
        ldx     TXTPTR
        inc     EOLPNTR
L24DB:
        iny
        lda     MATHTBL+28+1,y
        bpl     L24DB
        lda     TOKEN_NAME_TABLE,y
        bne     L2498
        lda     INPUTBUFFERX,x
        bpl     L24AA
; ---END OF LINE------------------
L24EA:
        sta     INPUTBUFFER-3,y
        lda     #lo(INPUTBUFFER)-1
        sta     TXTPTR
        rts
; ----------------------------------------------------------------------------
; SEARCH FOR LINE
;
; (LINNUM) = LINE # TO FIND
; IF NOT FOUND:  CARRY = 0
;	LOWTR POINTS AT NEXT LINE
; IF FOUND:      CARRY = 1
;	LOWTR POINTS AT LINE
; ----------------------------------------------------------------------------
FNDLIN:
        lda     TXTTAB
        ldx     TXTTAB+1
FL1:
        ldy     #$01
        sta     LOWTR
        stx     LOWTR+1
        lda     (LOWTR),y
        beq     L251F
        iny
        iny
        lda     LINNUM+1
        cmp     (LOWTR),y
        bcc     L2520
        beq     L250D
        dey
        bne     L2516
L250D:
        lda     LINNUM
        dey
        cmp     (LOWTR),y
        bcc     L2520
        beq     L2520
L2516:
        dey
        lda     (LOWTR),y
        tax
        dey
        lda     (LOWTR),y
        bcs     FL1
L251F:
        clc
L2520:
        rts
; ----------------------------------------------------------------------------
; "NEW" STATEMENT
; ----------------------------------------------------------------------------
NEW:
        bne     L2520
SCRTCH:
        lda     #$00
        tay
        sta     (TXTTAB),y
        iny
        sta     (TXTTAB),y
        lda     TXTTAB
        adc     #$02
        sta     VARTAB
        lda     TXTTAB+1
        adc     #$00
        sta     VARTAB+1
; ----------------------------------------------------------------------------
SETPTRS:
        jsr     STXTPT
; ----------------------------------------------------------------------------
; "CLEAR" STATEMENT
; ----------------------------------------------------------------------------
CLEARC:
        lda     MEMSIZ
        ldy     MEMSIZ+1
        sta     FRETOP
        sty     FRETOP+1
        lda     VARTAB
        ldy     VARTAB+1
        sta     ARYTAB
        sty     ARYTAB+1
        sta     STREND
        sty     STREND+1
        jsr     RESTORE
; ----------------------------------------------------------------------------
STKINI:
        ldx     #TEMPST
        stx     TEMPPT
        pla
        sta     STACK+STACK_TOP+1
        pla
        sta     STACK+STACK_TOP+2
        ldx     #STACK_TOP
        txs
        lda     #$00
        sta     OLDTEXT+1
        sta     SUBFLG
L256A:
        rts
; ----------------------------------------------------------------------------
; SET TXTPTR TO BEGINNING OF PROGRAM
; ----------------------------------------------------------------------------
STXTPT:
        clc
        lda     TXTTAB
        adc     #$FF
        sta     TXTPTR
        lda     TXTTAB+1
        adc     #$FF
        sta     TXTPTR+1
        rts
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; "LIST" STATEMENT
; ----------------------------------------------------------------------------
LIST:
        bcc     L2581
        beq     L2581
        cmp     #TOKEN_MINUS
        bne     L256A
L2581:
        jsr     LINGET
        jsr     FNDLIN
        jsr     CHRGOT
        beq     L2598
        cmp     #TOKEN_MINUS
        bne     L2520
        jsr     CHRGET
        jsr     LINGET
        bne     L2520
L2598:
        pla
        pla
        lda     LINNUM
        ora     LINNUM+1
        bne     L25A6
        lda     #$FF
        sta     LINNUM
        sta     LINNUM+1
L25A6:
        ldy     #$01
        sty     DATAFLG
        lda     (LOWTRX),y
        beq     L25E5
        jsr     ISCNTC
        jsr     CRDO
        iny
        lda     (LOWTRX),y
        tax
        iny
        lda     (LOWTRX),y
        cmp     LINNUM+1
        bne     L25C1
        cpx     LINNUM
        beq     L25C3
L25C1:
        bcs     L25E5
; ---LIST ONE LINE----------------
L25C3:
        sty     FORPNT
        jsr     LINPRT
        lda     #$20
L25CA:
        ldy     FORPNT
        and     #$7F
L25CE:
        jsr     OUTDO
        cmp     #$22
        bne     LA519
        lda     DATAFLG
        eor     #$FF
        sta     DATAFLG
LA519:
        iny
        lda     (LOWTRX),y
        bne     L25E8
        tay
        lda     (LOWTRX),y
        tax
        iny
        lda     (LOWTRX),y
        stx     LOWTRX
        sta     LOWTRX+1
        bne     L25A6
L25E5:
        jmp     RESTART
L25E8:
        bpl     L25CE
        cmp     #$FF
        beq     L25CE
        bit     DATAFLG
        bmi     L25CE
        sec
        sbc     #$7F
        tax
        sty     FORPNT
        ldy     #$FF
L25F2:
        dex
        beq     L25FD
L25F5:
        iny
        lda     TOKEN_NAME_TABLE,y
        bpl     L25F5
        bmi     L25F2
L25FD:
        iny
        lda     TOKEN_NAME_TABLE,y
        bmi     L25CA
        jsr     OUTDO
        bne     L25FD	; always
; ----------------------------------------------------------------------------
; "FOR" STATEMENT
;
; FOR PUSHES 18 BYTES ON THE STACK:
; 2 -- TXTPTR
; 2 -- LINE NUMBER
; 5 -- INITIAL (CURRENT)  FOR VARIABLE VALUE
; 1 -- STEP SIGN
; 5 -- STEP VALUE
; 2 -- ADDRESS OF FOR VARIABLE IN VARTAB
; 1 -- FOR TOKEN ($81)
; ----------------------------------------------------------------------------
FOR:
        lda     #$80
        sta     SUBFLG
        jsr     LET
        jsr     GTFORPNT
        bne     L2619
        txa
        adc     #FOR_STACK1
        tax
        txs
L2619:
        pla
        pla
        lda     #FOR_STACK2
        jsr     CHKMEM
        jsr     DATAN
        clc
        tya
        adc     TXTPTR
        pha
        lda     TXTPTR+1
        adc     #$00
        pha
        lda     CURLIN+1
        pha
        lda     CURLIN
        pha
        lda     #TOKEN_TO
        jsr     SYNCHR
        jsr     CHKNUM
        jsr     FRMNUM
        lda     FACSIGN
        ora     #$7F
        and     FAC+1
        sta     FAC+1
        lda     #lo(STEP)
        ldy     #hi(STEP)
        sta     INDEX
        sty     INDEX+1
        jmp     FRM_STACK3
; ----------------------------------------------------------------------------
; "STEP" PHRASE OF "FOR" STATEMENT
; ----------------------------------------------------------------------------
STEP:
        lda     #lo(CON_ONE)
        ldy     #hi(CON_ONE)
        jsr     LOAD_FAC_FROM_YA
        jsr     CHRGOT
        cmp     #TOKEN_STEP
        bne     L2665
        jsr     CHRGET
        jsr     FRMNUM
L2665:
        jsr     SIGN
        jsr     FRM_STACK2
        lda     FORPNT+1
        pha
        lda     FORPNT
        pha
        lda     #$81
        pha
; ----------------------------------------------------------------------------
; PERFORM NEXT STATEMENT
; ----------------------------------------------------------------------------
NEWSTT:
        jsr     ISCNTC
        lda     TXTPTR
        ldy     TXTPTR+1
        beq     L2683
        sta     OLDTEXT
        sty     OLDTEXT+1
        ldy     #$00
L2683:
        lda     (TXTPTR),y
        beq     LA5DC	; old: 1 cycle more on generic case
        cmp     #$3A
        beq     NEWSTT2
SYNERR1:
        jmp     SYNERR
LA5DC:
        ldy     #$02
        lda     (TXTPTR),y
        clc
        beq     L2701
        iny
        lda     (TXTPTR),y
        sta     CURLIN
        iny
        lda     (TXTPTR),y
        sta     CURLIN+1
        tya
        adc     TXTPTR
        sta     TXTPTR
        bcc     NEWSTT2
        inc     TXTPTR+1
NEWSTT2:
        jsr     CHRGET
        jsr     EXECUTE_STATEMENT
        jmp     NEWSTT
; ----------------------------------------------------------------------------
; EXECUTE A STATEMENT
;
; (A) IS FIRST CHAR OF STATEMENT
; CARRY IS SET
; ----------------------------------------------------------------------------
EXECUTE_STATEMENT:
        beq     RET1
        sec
EXECUTE_STATEMENT1:
        sbc     #$80
        bcs     *+5     
        jmp     LET	; old: 1 cycle more on instr.

        cmp     #NUM_TOKENS
        bcs     SYNERR1
        asl     a
        tay
        lda     TOKEN_ADDRESS_TABLE+1,y
        pha
        lda     TOKEN_ADDRESS_TABLE,y
        pha
        jmp     CHRGET
; ----------------------------------------------------------------------------
; "RESTORE" STATEMENT
; ----------------------------------------------------------------------------
RESTORE:
        sec
        lda     TXTTAB
        sbc     #$01
        ldy     TXTTAB+1
        bcs     SETDA
        dey
SETDA:
        sta     DATPTR
        sty     DATPTR+1
        rts
; ----------------------------------------------------------------------------
; SEE IF CONTROL-C TYPED
; ----------------------------------------------------------------------------

ISCNTC:
; MODIFIED CALL BY G.SEARLE FROM THE ORIGINAL OSI CODE
        jsr     MONISCNTC
; runs into "STOP"
; ----------------------------------------------------------------------------
; "STOP" STATEMENT
; ----------------------------------------------------------------------------
STOP:
        bcs     END2
; ----------------------------------------------------------------------------
; "END" STATEMENT
; ----------------------------------------------------------------------------
END:
        clc
END2:
        bne     RET1
        lda     TXTPTR
        ldy     TXTPTR+1
        beq     END4
        sta     OLDTEXT
        sty     OLDTEXT+1
CONTROL_C_TYPED:
        lda     CURLIN
        ldy     CURLIN+1
        sta     OLDLIN
        sty     OLDLIN+1
END4:
        pla
        pla
L2701:
        lda     #lo(QT_BREAK)
        ldy     #hi(QT_BREAK)
        ldx     #$00
        stx     Z14
        bcc     L270E
        jmp     PRINT_ERROR_LINNUM
L270E:
        jmp     RESTART
; ----------------------------------------------------------------------------
; "CONT" COMMAND
; ----------------------------------------------------------------------------
CONT:
        bne     RET1
        ldx     #ERR_CANTCONT
        ldy     OLDTEXT+1
        bne     L271C
        jmp     ERROR
L271C:
        lda     OLDTEXT
        sta     TXTPTR
        sty     TXTPTR+1
        lda     OLDLIN
        ldy     OLDLIN+1
        sta     CURLIN
        sty     CURLIN+1
RET1:
        rts
NULL:
        jsr     GETBYT
        bne     RET1
        inx
        cpx     #NULL_MAX
        bcs     L2739
        dex
        stx     Z15
        rts
L2739:
        jmp     IQERR
CLEAR:
        bne     RET1
        jmp     CLEARC
; ----------------------------------------------------------------------------
; "RUN" COMMAND
; ----------------------------------------------------------------------------
RUN:
        bne     L27CF
        jmp     SETPTRS
L27CF:
        jsr     CLEARC
        jmp     L27E9
; ----------------------------------------------------------------------------
; "GOSUB" STATEMENT
;
; LEAVES 7 BYTES ON STACK:
; 2 -- RETURN ADDRESS (NEWSTT)
; 2 -- TXTPTR
; 2 -- LINE #
; 1 -- GOSUB TOKEN
; ----------------------------------------------------------------------------
GOSUB:
        lda     #$03
        jsr     CHKMEM
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        lda     CURLIN+1
        pha
        lda     CURLIN
        pha
        lda     #TOKEN_GOSUB
        pha
L27E9:
        jsr     CHRGOT
        jsr     GOTO
        jmp     NEWSTT
; ----------------------------------------------------------------------------
; "GOTO" STATEMENT
; ALSO USED BY "RUN" AND "GOSUB"
; ----------------------------------------------------------------------------
GOTO:
        jsr     LINGET
        jsr     REMN
        lda     CURLIN+1
        cmp     LINNUM+1
        bcs     L2809
        tya
        sec
        adc     TXTPTR
        ldx     TXTPTR+1
        bcc     L280D
        inx
        bcs     L280D
L2809:
        lda     TXTTAB
        ldx     TXTTAB+1
L280D:
        jsr     FL1
        bcc     UNDERR
        lda     LOWTRX
        sbc     #$01
        sta     TXTPTR
        lda     LOWTRX+1
        sbc     #$00
        sta     TXTPTR+1
L281E:
        rts
; ----------------------------------------------------------------------------
; "POP" AND "RETURN" STATEMENTS
; ----------------------------------------------------------------------------
POP:
        bne     L281E
        lda     #$FF
        sta     FORPNT
        jsr     GTFORPNT
        txs
        cmp     #TOKEN_GOSUB
        beq     RETURN
        ldx     #ERR_NOGOSUB
        db   $2C
UNDERR:
        ldx     #ERR_UNDEFSTAT
        jmp     ERROR
; ----------------------------------------------------------------------------
SYNERR2:
        jmp     SYNERR
; ----------------------------------------------------------------------------
RETURN:
        pla
        pla
        sta     CURLIN
        pla
        sta     CURLIN+1
        pla
        sta     TXTPTR
        pla
        sta     TXTPTR+1
; ----------------------------------------------------------------------------
; "DATA" STATEMENT
; EXECUTED BY SKIPPING TO NEXT COLON OR EOL
; ----------------------------------------------------------------------------
DATA:
        jsr     DATAN
; ----------------------------------------------------------------------------
; ADD (Y) TO TXTPTR
; ----------------------------------------------------------------------------
ADDON:
        tya
        clc
        adc     TXTPTR
        sta     TXTPTR
        bcc     L2852
        inc     TXTPTR+1
L2852:
        rts
; ----------------------------------------------------------------------------
; SCAN AHEAD TO NEXT ":" OR EOL
; ----------------------------------------------------------------------------
DATAN:
        ldx     #$3A
        db   $2C
REMN:
        ldx     #$00
        stx     CHARAC
        ldy     #$00
        sty     ENDCHR
L285E:
        lda     ENDCHR
        ldx     CHARAC
        sta     CHARAC
        stx     ENDCHR
L2866:
        lda     (TXTPTR),y
        beq     L2852
        cmp     ENDCHR
        beq     L2852
        iny
        cmp     #$22
        beq     L285E; old: swap & cont is faster
        bne     L2866
; ----------------------------------------------------------------------------
; "IF" STATEMENT
; ----------------------------------------------------------------------------
IF:
        jsr     FRMEVL
        jsr     CHRGOT
        cmp     #TOKEN_GOTO
        beq     L2884
        lda     #TOKEN_THEN
        jsr     SYNCHR
L2884:
        lda     FAC
        bne     L288D
; ----------------------------------------------------------------------------
; "REM" STATEMENT, OR FALSE "IF" STATEMENT
; ----------------------------------------------------------------------------
REM:
        jsr     REMN
        beq     ADDON
L288D:
        jsr     CHRGOT
        bcs     L2895
        jmp     GOTO
L2895:
        jmp     EXECUTE_STATEMENT
; ----------------------------------------------------------------------------
; "ON" STATEMENT
;
; ON <EXP> GOTO <LIST>
; ON <EXP> GOSUB <LIST>
; ----------------------------------------------------------------------------
ON:
        jsr     GETBYT
        pha
        cmp     #TOKEN_GOSUB
        beq     L28A4
L28A0:
        cmp     #TOKEN_GOTO
        bne     SYNERR2
L28A4:
        dec     FAC_LAST
        bne     L28AC
        pla
        jmp     EXECUTE_STATEMENT1
L28AC:
        jsr     CHRGET
        jsr     LINGET
        cmp     #$2C
        beq     L28A4
        pla
L28B7:
        rts
; ----------------------------------------------------------------------------
; CONVERT LINE NUMBER
; ----------------------------------------------------------------------------
LINGET:
        ldx     #$00
        stx     LINNUM
        stx     LINNUM+1
L28BE:
        bcs     L28B7
        sbc     #$2F
        sta     CHARAC
        lda     LINNUM+1
        sta     INDEX
        cmp     #$19
        bcs     L28A0
; <<<<<DANGEROUS CODE>>>>>
; NOTE THAT IF (A) = $AB ON THE LINE ABOVE,
; ON.1 WILL COMPARE = AND CAUSE A CATASTROPHIC
; JUMP TO $22D9 (FOR GOTO), OR OTHER LOCATIONS
; FOR OTHER CALLS TO LINGET.
;
; YOU CAN SEE THIS IS YOU FIRST PUT "BRK" IN $22D9,
; THEN TYPE "GO TO 437761".
;
; ANY VALUE FROM 437760 THROUGH 440319 WILL CAUSE
; THE PROBLEM.  ($AB00 - $ABFF)
; <<<<<DANGEROUS CODE>>>>>
        lda     LINNUM
        asl     a
        rol     INDEX
        asl     a
        rol     INDEX
        adc     LINNUM
        sta     LINNUM
        lda     INDEX
        adc     LINNUM+1
        sta     LINNUM+1
        asl     LINNUM
        rol     LINNUM+1
        lda     LINNUM
        adc     CHARAC
        sta     LINNUM
        bcc     L28EC
        inc     LINNUM+1
L28EC:
        jsr     CHRGET
        jmp     L28BE
; ----------------------------------------------------------------------------
; "LET" STATEMENT
;
; LET <VAR> = <EXP>
; <VAR> = <EXP>
; ----------------------------------------------------------------------------
LET:
        jsr     PTRGET
        sta     FORPNT
        sty     FORPNT+1
        lda     #TOKEN_EQUAL
        jsr     SYNCHR
        lda     VALTYP
        pha
        jsr     FRMEVL
        pla
        rol     a
        jsr     CHKVAL
        bne     LETSTRING
; ----------------------------------------------------------------------------
; REAL VARIABLE = EXPRESSION
; ----------------------------------------------------------------------------
        jmp     SETFOR
LETSTRING:
; ----------------------------------------------------------------------------
; INSTALL STRING, DESCRIPTOR ADDRESS IS AT FAC+3,4
; ----------------------------------------------------------------------------
        ldy     #$02
        lda     (FAC_LAST-1),y
        cmp     FRETOP+1
        bcc     L2946
        bne     L2938
        dey
        lda     (FAC_LAST-1),y
        cmp     FRETOP
        bcc     L2946
L2938:
        ldy     FAC_LAST
        cpy     VARTAB+1
        bcc     L2946
        bne     L294D
        lda     FAC_LAST-1
        cmp     VARTAB
        bcs     L294D
L2946:
        lda     FAC_LAST-1
        ldy     FAC_LAST
        jmp     L2963
L294D:
        ldy     #$00
        lda     (FAC_LAST-1),y
        jsr     STRINI
        lda     DSCPTR
        ldy     DSCPTR+1
        sta     STRNG1
        sty     STRNG1+1
        jsr     MOVINS
        lda     #FAC
        ldy     #$00
L2963:
        sta     DSCPTR
        sty     DSCPTR+1
        jsr     FRETMS
        ldy     #$00
        lda     (DSCPTR),y
        sta     (FORPNT),y
        iny
        lda     (DSCPTR),y
        sta     (FORPNT),y
        iny
        lda     (DSCPTR),y
        sta     (FORPNT),y
        rts
PRSTRING:
        jsr     STRPRT
L297E:
        jsr     CHRGOT
; ----------------------------------------------------------------------------
; "PRINT" STATEMENT
; ----------------------------------------------------------------------------
PRINT:
        beq     CRDO
PRINT2:
        beq     L29DD
        cmp     #TOKEN_TAB
        beq     L29F5
        cmp     #TOKEN_SPC
        beq     L29F5
        cmp     #','
        beq     L29DE
        cmp     #$3B
        beq     L2A0D
        jsr     FRMEVL
        bit     VALTYP
        bmi     PRSTRING
        jsr     FOUT
        jsr     STRLIT
        ldy     #$00
        lda     (FAC_LAST-1),y
        clc
        adc     POSX
        cmp     Z17
        bcc     L29B1
        jsr     CRDO
L29B1:
        jsr     STRPRT
        jsr     OUTSP
        bne     L297E ; branch always
L29B9:
        ldy     #$00
        sty     INPUTBUFFER,x
        ldx     #LINNUM+1
CRDO:
        lda     #CRLF_1
        sta     POSX
        jsr     OUTDO
        lda     #CRLF_2
        jsr     OUTDO
PRINTNULLS:
        txa
        pha
        ldx     Z15
        beq     L29D9
        lda     #$00
L29D3:
        jsr     OUTDO
        dex
        bne     L29D3
L29D9:
        stx     POSX
        pla
        tax
L29DD:
        rts
L29DE:
        lda     POSX
        cmp     Z18
        bcc     L29EA
        jsr     CRDO
        jmp     L2A0D
L29EA:
        sec
L29EB:
        sbc     #$0E
        bcs     L29EB
        eor     #$FF
        adc     #$01
        bne     L2A08
L29F5:
        pha
        jsr     GTBYTC
        cmp     #')'
        bne     SYNERR4
        pla
        cmp     #TOKEN_TAB
        bne     L2A0A
        txa
        sbc     POSX
        bcc     L2A0D
        beq     L2A0D
L2A08:
        tax
L2A0A:
        jsr     OUTSP
        dex
        bne     L2A0A
L2A0D:
        jsr     CHRGET
        jmp     PRINT2
; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; ----------------------------------------------------------------------------
STROUT:
        jsr     STRLIT
; ----------------------------------------------------------------------------
; PRINT STRING AT (FACMO,FACLO)
; ----------------------------------------------------------------------------
STRPRT:
        jsr     FREFAC
        tax
        ldy     #$00
        inx
L2A22:
        dex
        beq     L29DD
        lda     (INDEX),y
        jsr     OUTDO
        iny
        cmp     #$0D
        bne     L2A22
        jsr     PRINTNULLS
        jmp     L2A22
; ----------------------------------------------------------------------------
OUTSP:
        lda     #$20
        db   $2C
OUTQUES:
        lda     #$3F
; ----------------------------------------------------------------------------
; PRINT CHAR FROM (A)
; ----------------------------------------------------------------------------
OUTDO:
        bit     Z14
        bmi     L2A56
; Commodore forgot to remove this in CBM1
        pha
        cmp     #$20
        bcc     L2A4E
        lda     POSX
        cmp     Z17
        bne     L2A4C
        jsr     CRDO
L2A4C:
        inc     POSX
L2A4E:
; Commodore forgot to remove this in CBM1
        pla
        jsr     MONCOUT
        nop
        nop
        nop
        nop
L2A56:
        and     #$FF
        rts
; ----------------------------------------------------------------------------
; ???
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; INPUT CONVERSION ERROR:  ILLEGAL CHARACTER
; IN NUMERIC FIELD.  MUST DISTINGUISH
; BETWEEN INPUT, READ, AND GET
; ----------------------------------------------------------------------------
INPUTERR:
        lda     INPUTFLG
        beq     RESPERR	; INPUT
; without this, it treats GET errors
; like READ errors
        lda     Z8C
        ldy     Z8C+1
        sta     CURLIN
        sty     CURLIN+1
SYNERR4:
        jmp     SYNERR
RESPERR:
        lda     #lo(ERRREENTRY)
        ldy     #hi(ERRREENTRY)
        jsr     STROUT
        lda     OLDTEXT
        ldy     OLDTEXT+1
        sta     TXTPTR
        sty     TXTPTR+1
        rts
; ----------------------------------------------------------------------------
; "GET" STATEMENT
; ----------------------------------------------------------------------------
GET:
; ----------------------------------------------------------------------------
; "INPUT#" STATEMENT
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; "INPUT" STATEMENT
; ----------------------------------------------------------------------------
INPUT:
        lsr     Z14
        cmp     #$22
        bne     L2A9E
        jsr     STRTXT
        lda     #$3B
        jsr     SYNCHR
        jsr     STRPRT
L2A9E:
        jsr     ERRDIR
        lda     #$2C
        sta     INPUTBUFFER-1
        jsr     NXIN
        lda     INPUTBUFFER
        bne     L2ABE
        clc
        jmp     CONTROL_C_TYPED
NXIN:
        jsr     OUTQUES	; '?'
        jsr     OUTSP
        jmp     INLIN
; ----------------------------------------------------------------------------
; "GETC" STATEMENT
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; "READ" STATEMENT
; ----------------------------------------------------------------------------
READ:
        ldx     DATPTR
        ldy     DATPTR+1
; AppleSoft II, too
        db   $A9	; LDA #$98
L2ABE:
        tya
; ----------------------------------------------------------------------------
; PROCESS INPUT LIST
;
; (Y,X) IS ADDRESS OF INPUT DATA STRING
; (A) = VALUE FOR INPUTFLG:  $00 FOR INPUT
; 				$40 FOR GET
;				$98 FOR READ
; ----------------------------------------------------------------------------
        sta     INPUTFLG
        stx     INPTR
        sty     INPTR+1
PROCESS_INPUT_ITEM:
        jsr     PTRGET
        sta     FORPNT
        sty     FORPNT+1
        lda     TXTPTR
        ldy     TXTPTR+1
        sta     TXPSV
        sty     TXPSV+1
        ldx     INPTR
        ldy     INPTR+1
        stx     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGOT
        bne     INSTART
        bit     INPUTFLG
; BUG: The beq/bne L2AF8 below is supposed
; to be always taken. For this to happen,
; the last load must be a 0 for beq
; and != 0 for bne. The original Microsoft
; code had ldx/ldy/bne here, which was only
; correct for a non-ZP INPUTBUFFER. Commodore
; fixed it in CBMBASIC V1 by swapping the
; ldx and the ldy. It was broken on KIM,
; but okay on APPLE and CBM2, because
; these used a non-ZP INPUTBUFFER.
; Microsoft fixed this somewhere after KIM
; and before MICROTAN, by using beq instead
; of bne in the ZP case.
        bmi     FINDATA
        jsr     OUTQUES
        jsr     NXIN
        stx     TXTPTR
        sty     TXTPTR+1
; ----------------------------------------------------------------------------
INSTART:
        jsr     CHRGET
        bit     VALTYP
        bpl     L2B34
        sta     CHARAC
        cmp     #$22
        beq     L2B1D
        lda     #$3A
        sta     CHARAC
        lda     #$2C
        clc
L2B1D:
        sta     ENDCHR
        lda     TXTPTR
        ldy     TXTPTR+1
        adc     #$00
        bcc     L2B28
        iny
L2B28:
        jsr     STRLT2
        jsr     POINT
        jsr     LETSTRING
        jmp     INPUT_MORE
; ----------------------------------------------------------------------------
L2B34:
        jsr     FIN
        jsr     SETFOR
; ----------------------------------------------------------------------------
INPUT_MORE:
        jsr     CHRGOT
        beq     L2B48
        cmp     #$2C
        beq     L2B48
        jmp     INPUTERR
L2B48:
        lda     TXTPTR
        ldy     TXTPTR+1
        sta     INPTR
        sty     INPTR+1
        lda     TXPSV
        ldy     TXPSV+1
        sta     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGOT
        beq     INPDONE
        jsr     CHKCOM
        jmp     PROCESS_INPUT_ITEM
; ----------------------------------------------------------------------------
FINDATA:
        jsr     DATAN
        iny
        tax
        bne     L2B7C
        ldx     #ERR_NODATA
        iny
        lda     (TXTPTR),y
        beq     GERR
        iny
        lda     (TXTPTR),y
        sta     Z8C
        iny
        lda     (TXTPTR),y
        iny
        sta     Z8C+1
L2B7C:
        lda     (TXTPTR),y
        tax
        jsr     ADDON
        cpx     #$83
        bne     FINDATA
        jmp     INSTART
; ---NO MORE INPUT REQUESTED------
INPDONE:
        lda     INPTR
        ldy     INPTR+1
        ldx     INPUTFLG
        beq     L2B94 ; INPUT
        jmp     SETDA
L2B94:
        ldy     #$00
        lda     (INPTR),y
        beq     L2BA1
        lda     #lo(ERREXTRA)
        ldy     #hi(ERREXTRA)
        jmp     STROUT
L2BA1:
        rts
; ----------------------------------------------------------------------------
ERREXTRA:
        db   "?EXTRA IGNORED"


        db   $0D,$0A,$00
ERRREENTRY:
        db   "?REDO FROM START"


        db   $0D,$0A,$00
; ----------------------------------------------------------------------------
; "NEXT" STATEMENT
; ----------------------------------------------------------------------------
NEXT:
        bne     NEXT1
        ldy     #$00
        beq     NEXT2
NEXT1:
        jsr     PTRGET
NEXT2:
        sta     FORPNT
        sty     FORPNT+1
        jsr     GTFORPNT
        beq     NEXT3
        ldx     #$00
GERR:
        beq     JERROR
NEXT3:
        txs
        inx
        inx
        inx
        inx
        txa
        inx
        inx
        inx
        inx
        inx
        stx     DEST
        ldy     #hi(STACK)
        jsr     LOAD_FAC_FROM_YA
        tsx
        lda     STACK+BYTES_FP+4,x
        sta     FACSIGN
        lda     FORPNT
        ldy     FORPNT+1
        jsr     FADD
        jsr     SETFOR
        ldy     #hi(STACK)
        jsr     FCOMP2
        tsx
        sec
        sbc     STACK+BYTES_FP+4,x
        beq     L2C22
        lda     STACK+2*BYTES_FP+5,x
        sta     CURLIN
        lda     STACK+2*BYTES_FP+6,x
        sta     CURLIN+1
        lda     STACK+2*BYTES_FP+8,x
        sta     TXTPTR
        lda     STACK+2*BYTES_FP+7,x
        sta     TXTPTR+1
L2C1F:
        jmp     NEWSTT
L2C22:
        txa
        adc     #2*BYTES_FP+7
        tax
        txs
        jsr     CHRGOT
        cmp     #$2C
        bne     L2C1F
        jsr     CHRGET
        jsr     NEXT1
; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION, MAKE SURE IT IS NUMERIC
; ----------------------------------------------------------------------------
FRMNUM:
        jsr     FRMEVL
; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS NUMERIC
; ----------------------------------------------------------------------------
CHKNUM:
        clc
        db   $24
; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS STRING
; ----------------------------------------------------------------------------
CHKSTR:
        sec
; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS CORRECT TYPE
; IF C=0, TYPE MUST BE NUMERIC
; IF C=1, TYPE MUST BE STRING
; ----------------------------------------------------------------------------
CHKVAL:
        bit     VALTYP
        bmi     L2C41
        bcs     L2C43
L2C40:
        rts
L2C41:
        bcs     L2C40
L2C43:
        ldx     #ERR_BADTYPE
JERROR:
        jmp     ERROR
; ----------------------------------------------------------------------------
; EVALUATE THE EXPRESSION AT TXTPTR, LEAVING THE
; RESULT IN FAC.  WORKS FOR BOTH STRING AND NUMERIC
; EXPRESSIONS.
; ----------------------------------------------------------------------------
FRMEVL:
        ldx     TXTPTR
        bne     L2C4E
        dec     TXTPTR+1
L2C4E:
        dec     TXTPTR
        ldx     #$00
        db   $24
FRMEVL1:
        pha
        txa
        pha
        lda     #$01
        jsr     CHKMEM
        jsr     FRM_ELEMENT
        lda     #$00
        sta     CPRTYP
FRMEVL2:
        jsr     CHRGOT
L2C65:
        sec
        sbc     #TOKEN_GREATER
        bcc     L2C81
        cmp     #$03
        bcs     L2C81
        cmp     #$01
        rol     a
        eor     #$01
        eor     CPRTYP
        cmp     CPRTYP
        bcc     SNTXERR
        sta     CPRTYP
        jsr     CHRGET
        jmp     L2C65
L2C81:
        ldx     CPRTYP
        bne     FRM_RELATIONAL
        bcs     L2D02
        adc     #$07
        bcc     L2D02
        adc     VALTYP
        bne     L2C92
        jmp     CAT
L2C92:
        adc     #$FF
        sta     INDEX
        asl     a
        adc     INDEX
        tay
FRM_PRECEDENCE_TEST:
        pla
        cmp     MATHTBL,y
        bcs     FRM_PERFORM1
        jsr     CHKNUM
L2CA3:
        pha
L2CA4:
        jsr     FRM_RECURSE
        pla
        ldy     LASTOP
        bpl     PREFNC
        tax
        beq     GOEX
        bne     FRM_PERFORM2
; ----------------------------------------------------------------------------
; FOUND ONE OR MORE RELATIONAL OPERATORS <,=,>
; ----------------------------------------------------------------------------
FRM_RELATIONAL:
        lsr     VALTYP
        txa
        rol     a
        ldx     TXTPTR
        bne     L2CBB
        dec     TXTPTR+1
L2CBB:
        dec     TXTPTR
        ldy     #$1B
        sta     CPRTYP
        bne     FRM_PRECEDENCE_TEST
PREFNC:
        cmp     MATHTBL,y
        bcs     FRM_PERFORM2
        bcc     L2CA3
; ----------------------------------------------------------------------------
; STACK THIS OPERATION AND CALL FRMEVL FOR
; ANOTHER ONE
; ----------------------------------------------------------------------------
FRM_RECURSE:
        lda     MATHTBL+2,y
        pha
        lda     MATHTBL+1,y
        pha
        jsr     FRM_STACK1
        lda     CPRTYP
        jmp     FRMEVL1
SNTXERR:
        jmp     SYNERR
; ----------------------------------------------------------------------------
; STACK (FAC)
; THREE ENTRY POINTS:
; 	1, FROM FRMEVL
;	2, FROM "STEP"
;	3, FROM "FOR"
; ----------------------------------------------------------------------------
FRM_STACK1:
        lda     FACSIGN
        ldx     MATHTBL,y
; ----------------------------------------------------------------------------
; ENTER HERE FROM "STEP", TO PUSH STEP SIGN AND VALUE
; ----------------------------------------------------------------------------
FRM_STACK2:
        tay
        pla
        sta     INDEX
        inc     INDEX ; bug: assumes not on page boundary
; bug exists on AppleSoft II
        pla
        sta     INDEX+1
        tya
        pha
; ----------------------------------------------------------------------------
; ENTER HERE FROM "FOR", WITH (INDEX) = STEP,
; TO PUSH INITIAL VALUE OF "FOR" VARIABLE
; ----------------------------------------------------------------------------
FRM_STACK3:
        jsr     ROUND_FAC
        lda     FAC+3
        pha
        lda     FAC+2
        pha
        lda     FAC+1
        pha
        lda     FAC
        pha
        jmp     (INDEX)
L2D02:
        ldy     #$FF
        pla
GOEX:
        beq     EXIT
; ----------------------------------------------------------------------------
; PERFORM STACKED OPERATION
;
; (A) = PRECEDENCE BYTE
; STACK:  1 -- CPRMASK
;	5 -- (ARG)
;	2 -- ADDR OF PERFORMER
; ----------------------------------------------------------------------------
FRM_PERFORM1:
        cmp     #$64
        beq     L2D0E
        jsr     CHKNUM
L2D0E:
        sty     LASTOP
FRM_PERFORM2:
        pla
        lsr     a
        sta     CPRMASK
        pla
        sta     ARG
        pla
        sta     ARG+1
        pla
        sta     ARG+2
        pla
        sta     ARG+3
        pla
        sta     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
EXIT:
        lda     FAC
        rts
; ----------------------------------------------------------------------------
; GET ELEMENT IN EXPRESSION
;
; GET VALUE OF VARIABLE OR NUMBER AT TXTPNT, OR POINT
; TO STRING DESCRIPTOR IF A STRING, AND PUT IN FAC.
; ----------------------------------------------------------------------------
FRM_ELEMENT:
        lda     #$00
        sta     VALTYP
L2D31:
        jsr     CHRGET
        bcs     L2D39
L2D36:
        jmp     FIN
L2D39:
        jsr     ISLETC
        bcs     FRM_VARIABLE
CON_PI:
        cmp     #$2E
        beq     L2D36
        cmp     #TOKEN_MINUS
        beq     MIN
        cmp     #TOKEN_PLUS
        beq     L2D31
        cmp     #$22
        bne     NOT_
; ----------------------------------------------------------------------------
; STRING CONSTANT ELEMENT
;
; SET Y,A = (TXTPTR)+CARRY
; ----------------------------------------------------------------------------
STRTXT:
        lda     TXTPTR
        ldy     TXTPTR+1
        adc     #$00
        bcc     L2D57
        iny
L2D57:
        jsr     STRLIT
        jmp     POINT
; ----------------------------------------------------------------------------
; "NOT" FUNCTION
; IF FAC=0, RETURN FAC=1
; IF FAC<>0, RETURN FAC=0
; ----------------------------------------------------------------------------
NOT_:
        cmp     #TOKEN_NOT
        bne     L2D74
        ldy     #$18
        bne     EQUL
; ----------------------------------------------------------------------------
; COMPARISON FOR EQUALITY (= OPERATOR)
; ALSO USED TO EVALUATE "NOT" FUNCTION
; ----------------------------------------------------------------------------
EQUOP:
        jsr     AYINT
        lda     FAC_LAST
        eor     #$FF
        tay
        lda     FAC_LAST-1
        eor     #$FF
        jmp     GIVAYF
L2D74:
        cmp     #TOKEN_FN
        bne     L2D7B
        jmp     L31F3
L2D7B:
        cmp     #TOKEN_SGN
        bcc     PARCHK
        jmp     UNARY
; ----------------------------------------------------------------------------
; EVALUATE "(EXPRESSION)"
; ----------------------------------------------------------------------------
PARCHK:
        jsr     CHKOPN
        jsr     FRMEVL
CHKCLS:
        lda     #$29
        db   $2C
CHKOPN:
        lda     #$28
        db   $2C
CHKCOM:
        lda     #$2C
; ----------------------------------------------------------------------------
; UNLESS CHAR AT TXTPTR = (A), SYNTAX ERROR
; ----------------------------------------------------------------------------
SYNCHR:	; XXX all CBM code calls SYNCHR instead of CHKCOM
        ldy     #$00
        cmp     (TXTPTR),y
        bne     SYNERR
        jmp     CHRGET
; ----------------------------------------------------------------------------
SYNERR:
        ldx     #ERR_SYNTAX
        jmp     ERROR
; ----------------------------------------------------------------------------
MIN:
        ldy     #$15
EQUL:
        pla
        pla
        jmp     L2CA4
; ----------------------------------------------------------------------------
FRM_VARIABLE:
        jsr     PTRGET
FRM_VARIABLE_CALL	= *-1
        sta     FAC_LAST-1
        sty     FAC_LAST
        ldx     VALTYP
        beq     L2DB1
; bugfix?
; fixed on AppleSoft II, not on any CBM
        rts
L2DB1:
        jmp     LOAD_FAC_FROM_YA
; ----------------------------------------------------------------------------
UNARY:
        asl     a
        pha
        tax
        jsr     CHRGET
        cpx     #lo(TOKEN_LEFTSTR*2-1)
        bcc     L2DEF
        jsr     CHKOPN
        jsr     FRMEVL
        jsr     CHKCOM
        jsr     CHKSTR
        pla
        tax
        lda     FAC_LAST
        pha
        lda     FAC_LAST-1
        pha
        txa
        pha
        jsr     GETBYT
        pla
        tay
        txa
        pha
        jmp     L2DF4
L2DEF:
        jsr     PARCHK
        pla
        tay
L2DF4:
        lda     UNFNC+($80-TOKEN_SGN)*2,y
        sta     JMPADRS+1
        lda     UNFNC+($80-TOKEN_SGN)*2+1,y
        sta     JMPADRS+2
        jsr     JMPADRS
        jmp     CHKNUM
; ----------------------------------------------------------------------------
OR:
        ldy     #$FF
        db   $2C
; ----------------------------------------------------------------------------
TAND:
        ldy     #$00
        sty     EOLPNTR
        jsr     AYINT
        lda     FAC_LAST-1
        eor     EOLPNTR
        sta     CHARAC
        lda     FAC_LAST
        eor     EOLPNTR
        sta     ENDCHR
        jsr     COPY_ARG_TO_FAC
        jsr     AYINT
        lda     FAC_LAST
        eor     EOLPNTR
        and     ENDCHR
        eor     EOLPNTR
        tay
        lda     FAC_LAST-1
        eor     EOLPNTR
        and     CHARAC
        eor     EOLPNTR
        jmp     GIVAYF
; ----------------------------------------------------------------------------
; PERFORM RELATIONAL OPERATIONS
; ----------------------------------------------------------------------------
RELOPS:
        jsr     CHKVAL
        bcs     STRCMP
        lda     ARGSIGN
        ora     #$7F
        and     ARG+1
        sta     ARG+1
        lda     #lo(ARG)
        ldy     #$00
        jsr     FCOMP
        tax
        jmp     NUMCMP
; ----------------------------------------------------------------------------
; STRING COMPARISON
; ----------------------------------------------------------------------------
STRCMP:
        lda     #$00
        sta     VALTYP
        dec     CPRTYP
        jsr     FREFAC
        sta     FAC
        stx     FAC+1
        sty     FAC+2
        lda     ARG_LAST-1
        ldy     ARG_LAST
        jsr     FRETMP
        stx     ARG_LAST-1
        sty     ARG_LAST
        tax
        sec
        sbc     FAC
        beq     L2E74
        lda     #$01
        bcc     L2E74
        ldx     FAC
        lda     #$FF
L2E74:
        sta     FACSIGN
        ldy     #$FF
        inx
STRCMP1:
        iny
        dex
        bne     L2E84
        ldx     FACSIGN
NUMCMP:
        bmi     CMPDONE
        clc
        bcc     CMPDONE
L2E84:
        lda     (ARG_LAST-1),y
        cmp     (FAC+1),y
        beq     STRCMP1
        ldx     #$FF
        bcs     CMPDONE
        ldx     #$01
CMPDONE:
        inx
        txa
        rol     a
        and     CPRMASK
        beq     L2E99
        lda     #$FF
L2E99:
        jmp     FLOAT
; ----------------------------------------------------------------------------
; "DIM" STATEMENT
; ----------------------------------------------------------------------------
NXDIM:
        jsr     CHKCOM
DIM:
        tax
        jsr     PTRGET2
        jsr     CHRGOT
        bne     NXDIM
        rts
; ----------------------------------------------------------------------------
; PTRGET -- GENERAL VARIABLE SCAN
;
; SCANS VARIABLE NAME AT TXTPTR, AND SEARCHES THE
; VARTAB AND ARYTAB FOR THE NAME.
; IF NOT FOUND, CREATE VARIABLE OF APPROPRIATE TYPE.
; RETURN WITH ADDRESS IN VARPNT AND Y,A
;
; ACTUAL ACTIVITY CONTROLLED SOMEWHAT BY TWO FLAGS:
;	DIMFLG -- NONZERO IF CALLED FROM "DIM"
;		ELSE = 0
;
;	SUBFLG -- = $00
;		= $40 IF CALLED FROM "GETARYPT"
; ----------------------------------------------------------------------------
PTRGET:
        ldx     #$00
        jsr     CHRGOT
PTRGET2:
        stx     DIMFLG
PTRGET3:
        sta     VARNAM
        jsr     CHRGOT
        jsr     ISLETC
        bcs     NAMOK
        jmp     SYNERR
NAMOK:
        ldx     #$00
        stx     VALTYP
        jsr     CHRGET
        bcc     L2ECD
        jsr     ISLETC
        bcc     L2ED8
L2ECD:
        tax
L2ECE:
        jsr     CHRGET
        bcc     L2ECE
        jsr     ISLETC
        bcs     L2ECE
L2ED8:
        cmp     #$24
        bne     L2EF9
        lda     #$FF
        sta     VALTYP
        txa
        ora     #$80
        tax
        jsr     CHRGET
L2EF9:
        stx     VARNAM+1
        sec
        ora     SUBFLG
        sbc     #$28
        bne     L2F05
        jmp     ARRAY
L2F05:
        lda     #$00
        sta     SUBFLG
        lda     VARTAB
        ldx     VARTAB+1
        ldy     #$00
L2F0F:
        stx     LOWTR+1
L2F11:
        sta     LOWTR
        cpx     ARYTAB+1
        bne     L2F1B
        cmp     ARYTAB
        beq     NAMENOTFOUND
L2F1B:
        lda     VARNAM
        cmp     (LOWTR),y
        bne     L2F29
        lda     VARNAM+1
        iny
        cmp     (LOWTR),y
        beq     SET_VARPNT_AND_YA
        dey
L2F29:
        clc
        lda     LOWTR
        adc     #BYTES_PER_VARIABLE
        bcc     L2F11
        inx
        bne     L2F0F
; ----------------------------------------------------------------------------
; CHECK IF (A) IS ASCII LETTER A-Z
;
; RETURN CARRY = 1 IF A-Z
;	= 0 IF NOT
; ----------------------------------------------------------------------------
ISLETC:
        cmp     #$41
        bcc     L2F3C
        sbc     #$5B
        sec
        sbc     #$A5
L2F3C:
        rts
; ----------------------------------------------------------------------------
; VARIABLE NOT FOUND, SO MAKE ONE
; ----------------------------------------------------------------------------
NAMENOTFOUND:
        pla
        pha
        cmp     #lo(FRM_VARIABLE_CALL)
        bne     MAKENEWVARIABLE
        lda     #lo(C_ZERO)
        ldy     #hi(C_ZERO)
        rts
; ----------------------------------------------------------------------------
C_ZERO:
        db   $00,$00
; ----------------------------------------------------------------------------
; MAKE A NEW SIMPLE VARIABLE
;
; MOVE ARRAYS UP 7 BYTES TO MAKE ROOM FOR NEW VARIABLE
; ENTER 7-BYTE VARIABLE DATA IN THE HOLE
; ----------------------------------------------------------------------------
MAKENEWVARIABLE:
        lda     ARYTAB
        ldy     ARYTAB+1
        sta     LOWTR
        sty     LOWTR+1
        lda     STREND
        ldy     STREND+1
        sta     HIGHTR
        sty     HIGHTR+1
        clc
        adc     #BYTES_PER_VARIABLE
        bcc     L2F68
        iny
L2F68:
        sta     HIGHDS
        sty     HIGHDS+1
        jsr     BLTU
        lda     HIGHDS
        ldy     HIGHDS+1
        iny
        sta     ARYTAB
        sty     ARYTAB+1
        ldy     #$00
        lda     VARNAM
        sta     (LOWTR),y
        iny
        lda     VARNAM+1
        sta     (LOWTR),y
        lda     #$00
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y
; ----------------------------------------------------------------------------
; PUT ADDRESS OF VALUE OF VARIABLE IN VARPNT AND Y,A
; ----------------------------------------------------------------------------
SET_VARPNT_AND_YA:
        lda     LOWTR
        clc
        adc     #$02
        ldy     LOWTR+1
        bcc     L2F9E
        iny
L2F9E:
        sta     VARPNT
        sty     VARPNT+1
        rts
; ----------------------------------------------------------------------------
; COMPUTE ADDRESS OF FIRST VALUE IN ARRAY
; ARYPNT = (LOWTR) + #DIMS*2 + 5
; ----------------------------------------------------------------------------
GETARY:
        lda     EOLPNTR
        asl     a
        adc     #$05
        adc     LOWTR
        ldy     LOWTR+1
        bcc     L2FAF
        iny
L2FAF:
        sta     HIGHDS
        sty     HIGHDS+1
        rts
; ----------------------------------------------------------------------------
NEG32768:
        db   $90,$80,$00,$00
; ----------------------------------------------------------------------------
; EVALUATE NUMERIC FORMULA AT TXTPTR
; CONVERTING RESULT TO INTEGER 0 <= X <= 32767
; IN FAC+3,4
; ----------------------------------------------------------------------------
MAKINT:
        jsr     CHRGET
        jsr     FRMNUM
; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE POSITIVE AND LESS THAN 32768
; ----------------------------------------------------------------------------
MKINT:
        lda     FACSIGN
        bmi     MI1
; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE -32767 <= FAC <= 32767
; ----------------------------------------------------------------------------
AYINT:
        lda     FAC
        cmp     #$90
        bcc     MI2
        lda     #lo(NEG32768)
        ldy     #hi(NEG32768)
        jsr     FCOMP
MI1:
        bne     IQERR
MI2:
        jmp     QINT
; ----------------------------------------------------------------------------
; LOCATE ARRAY ELEMENT OR CREATE AN ARRAY
; ----------------------------------------------------------------------------
ARRAY:
        lda     DIMFLG
        pha
        lda     VALTYP
        pha
        ldy     #$00
L2FDE:
        tya
        pha
        lda     VARNAM+1
        pha
        lda     VARNAM
        pha
        jsr     MAKINT
        pla
        sta     VARNAM
        pla
        sta     VARNAM+1
        pla
        tay
        tsx
        lda     STACK+2,x
        pha
        lda     STACK+1,x
        pha
        lda     FAC_LAST-1
        sta     STACK+2,x
        lda     FAC_LAST
        sta     STACK+1,x
        iny
        jsr     CHRGOT
        cmp     #$2C
        beq     L2FDE
        sty     EOLPNTR
        jsr     CHKCLS
        pla
        sta     VALTYP
        pla
        sta     DIMFLG
; ----------------------------------------------------------------------------
; SEARCH ARRAY TABLE FOR THIS ARRAY NAME
; ----------------------------------------------------------------------------
        ldx     ARYTAB
        lda     ARYTAB+1
L301F:
        stx     LOWTR
        sta     LOWTR+1
        cmp     STREND+1
        bne     L302B
        cpx     STREND
        beq     MAKE_NEW_ARRAY
L302B:
        ldy     #$00
        lda     (LOWTR),y
        iny
        cmp     VARNAM
        bne     L303A
        lda     VARNAM+1
        cmp     (LOWTR),y
        beq     USE_OLD_ARRAY
L303A:
        iny
        lda     (LOWTR),y
        clc
        adc     LOWTR
        tax
        iny
        lda     (LOWTR),y
        adc     LOWTR+1
        bcc     L301F
; ----------------------------------------------------------------------------
; ERROR:  BAD SUBSCRIPTS
; ----------------------------------------------------------------------------
SUBERR:
        ldx     #ERR_BADSUBS
        db   $2C
; ----------------------------------------------------------------------------
; ERROR:  ILLEGAL QUANTITY
; ----------------------------------------------------------------------------
IQERR:
        ldx     #ERR_ILLQTY
JER:
        jmp     ERROR
; ----------------------------------------------------------------------------
; FOUND THE ARRAY
; ----------------------------------------------------------------------------
USE_OLD_ARRAY:
        ldx     #ERR_REDIMD
        lda     DIMFLG
        bne     JER
        jsr     GETARY
        lda     EOLPNTR
        ldy     #$04
        cmp     (LOWTR),y
        bne     SUBERR
        jmp     FIND_ARRAY_ELEMENT
; ----------------------------------------------------------------------------
; CREATE A NEW ARRAY, UNLESS CALLED FROM GETARYPT
; ----------------------------------------------------------------------------
MAKE_NEW_ARRAY:
        jsr     GETARY
        jsr     REASON
        lda     #$00
        tay
        sta     STRNG2+1
        ldx     #BYTES_PER_ELEMENT
        stx     STRNG2
        lda     VARNAM
        sta     (LOWTR),y
        iny
        lda     VARNAM+1
        sta     (LOWTR),y
        lda     EOLPNTR
        iny
        iny
        iny
        sta     (LOWTR),y
L308A:
        ldx     #$0B
        lda     #$00
        bit     DIMFLG
        bvc     L309A
        pla
        clc
        adc     #$01
        tax
        pla
        adc     #$00
L309A:
        iny
        sta     (LOWTR),y
        iny
        txa
        sta     (LOWTR),y
        jsr     MULTIPLY_SUBSCRIPT
        stx     STRNG2
        sta     STRNG2+1
        ldy     INDEX
        dec     EOLPNTR
        bne     L308A
        adc     HIGHDS+1
        bcs     GME
        sta     HIGHDS+1
        tay
        txa
        adc     HIGHDS
        bcc     L30BD
        iny
        beq     GME
L30BD:
        jsr     REASON
        sta     STREND
        sty     STREND+1
        lda     #$00
        inc     STRNG2+1
        ldy     STRNG2
        beq     L30D1
L30CC:
        dey
        sta     (HIGHDS),y
        bne     L30CC
L30D1:
        dec     HIGHDS+1
        dec     STRNG2+1
        bne     L30CC
        inc     HIGHDS+1
        sec
        lda     STREND
        sbc     LOWTR
        ldy     #$02
        sta     (LOWTR),y
        lda     STREND+1
        iny
        sbc     LOWTR+1
        sta     (LOWTR),y
        lda     DIMFLG
        bne     RTS9
        iny
; ----------------------------------------------------------------------------
; FIND SPECIFIED ARRAY ELEMENT
;
; (LOWTR),Y POINTS AT # OF DIMS IN ARRAY DESCRIPTOR
; THE SUBSCRIPTS ARE ALL ON THE STACK AS INTEGERS
; ----------------------------------------------------------------------------
FIND_ARRAY_ELEMENT:
        lda     (LOWTR),y
        sta     EOLPNTR
        lda     #$00
        sta     STRNG2
L30F6:
        sta     STRNG2+1
        iny
        pla
        tax
        sta     FAC_LAST-1
        pla
        sta     FAC_LAST
        cmp     (LOWTR),y
        bcc     FAE2
        bne     GSE
        iny
        txa
        cmp     (LOWTR),y
        bcc     FAE3
; ----------------------------------------------------------------------------
GSE:
        jmp     SUBERR
GME:
        jmp     MEMERR
; ----------------------------------------------------------------------------
FAE2:
        iny
FAE3:
        lda     STRNG2+1
        ora     STRNG2
        clc
        beq     L3124
        jsr     MULTIPLY_SUBSCRIPT
        txa
        adc     FAC_LAST-1
        tax
        tya
        ldy     INDEX
L3124:
        adc     FAC_LAST
        stx     STRNG2
        dec     EOLPNTR
        bne     L30F6
        asl     STRNG2
        rol     a
        bcs     GSE
        asl     STRNG2
        rol     a
        bcs     GSE
        tay
        lda     STRNG2
        adc     HIGHDS
        sta     VARPNT
        tya
        adc     HIGHDS+1
        sta     VARPNT+1
        tay
        lda     VARPNT
RTS9:
        rts
; ----------------------------------------------------------------------------
; MULTIPLY (STRNG2) BY ((LOWTR),Y)
; LEAVING PRODUCT IN A,X.  (HI-BYTE ALSO IN Y.)
; USED ONLY BY ARRAY SUBSCRIPT ROUTINES
; ----------------------------------------------------------------------------
MULTIPLY_SUBSCRIPT:
        sty     INDEX
        lda     (LOWTR),y
        sta     RESULT_LAST-2
        dey
        lda     (LOWTR),y
        sta     RESULT_LAST-1
        lda     #$10
        sta     INDX
        ldx     #$00
        ldy     #$00
L3163:
        txa
        asl     a
        tax
        tya
        rol     a
        tay
        bcs     GME
        asl     STRNG2
        rol     STRNG2+1
        bcc     L317C
        clc
        txa
        adc     RESULT_LAST-2
        tax
        tya
        adc     RESULT_LAST-1
        tay
        bcs     GME
L317C:
        dec     INDX
        bne     L3163
        rts
; ----------------------------------------------------------------------------
; "FRE" FUNCTION
;
; COLLECTS GARBAGE AND RETURNS # BYTES OF MEMORY LEFT
; ----------------------------------------------------------------------------
FRE:
        lda     VALTYP
        beq     L3188
        jsr     FREFAC
L3188:
        jsr     GARBAG
        sec
        lda     FRETOP
        sbc     STREND
        tay
        lda     FRETOP+1
        sbc     STREND+1
; FALL INTO GIVAYF TO FLOAT THE VALUE
; NOTE THAT VALUES OVER 32767 WILL RETURN AS NEGATIVE
; ----------------------------------------------------------------------------
; FLOAT THE SIGNED INTEGER IN A,Y
; ----------------------------------------------------------------------------
GIVAYF:
        ldx     #$00
        stx     VALTYP
        sta     FAC+1
        sty     FAC+2
        ldx     #$90
        jmp     FLOAT1
POS:
        ldy     POSX
; ----------------------------------------------------------------------------
; FLOAT (Y) INTO FAC, GIVING VALUE 0-255
; ----------------------------------------------------------------------------
SNGFLT:
        lda     #$00
        beq     GIVAYF
; ----------------------------------------------------------------------------
; CHECK FOR DIRECT OR RUNNING MODE
; GIVING ERROR IF DIRECT MODE
; ----------------------------------------------------------------------------
ERRDIR:
        ldx     CURLIN+1
        inx
        bne     RTS9
        ldx     #ERR_ILLDIR
L31AF:
        jmp     ERROR
DEF:
        jsr     FNC
        jsr     ERRDIR
        jsr     CHKOPN
        lda     #$80
        sta     SUBFLG
        jsr     PTRGET
        jsr     CHKNUM
        jsr     CHKCLS
        lda     #TOKEN_EQUAL
        jsr     SYNCHR
        lda     VARPNT+1
        pha
        lda     VARPNT
        pha
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        jsr     DATA
        jmp     L3250
FNC:
        lda     #TOKEN_FN
        jsr     SYNCHR
        ora     #$80
        sta     SUBFLG
        jsr     PTRGET3
        sta     FNCNAM
        sty     FNCNAM+1
        jmp     CHKNUM
L31F3:
        jsr     FNC
        lda     FNCNAM+1
        pha
        lda     FNCNAM
        pha
        jsr     PARCHK
        jsr     CHKNUM
        pla
        sta     FNCNAM
        pla
        sta     FNCNAM+1
        ldy     #$02
        ldx     #ERR_UNDEFFN
        lda     (FNCNAM),y
        beq     L31AF
        sta     VARPNT
        tax
        iny
        lda     (FNCNAM),y
        sta     VARPNT+1
L3219:
        lda     (VARPNT),y
        pha
        dey
        bpl     L3219
        ldy     VARPNT+1
        jsr     STORE_FAC_AT_YX_ROUNDED
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        lda     (FNCNAM),y
        sta     TXTPTR
        iny
        lda     (FNCNAM),y
        sta     TXTPTR+1
        lda     VARPNT+1
        pha
        lda     VARPNT
        pha
        jsr     FRMNUM
        pla
        sta     FNCNAM
        pla
        sta     FNCNAM+1
        jsr     CHRGOT
        beq     L324A
        jmp     SYNERR
L324A:
        pla
        sta     TXTPTR
        pla
        sta     TXTPTR+1
L3250:
        ldy     #$00
        pla
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        rts
; ----------------------------------------------------------------------------
; "STR$" FUNCTION
; ----------------------------------------------------------------------------
STR:
        jsr     CHKNUM
        ldy     #$00
        jsr     FOUT1
        pla
        pla
        lda     #$FF
        ldy     #$00
        beq     STRLIT
; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN FAC+3,4 AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRINI:
        ldx     FAC_LAST-1
        ldy     FAC_LAST
        stx     DSCPTR
        sty     DSCPTR+1
; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN Y,X AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRSPA:
        jsr     GETSPA
        stx     FAC+1
        sty     FAC+2
        sta     FAC
        rts
; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00 OR QUOTATION MARK
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLIT:
        ldx     #$22
        stx     CHARAC
        stx     ENDCHR
; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00, (CHARAC), OR (ENDCHR)
;
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLT2:
        sta     STRNG1
        sty     STRNG1+1
        sta     FAC+1
        sty     FAC+2
        ldy     #$FF
L3298:
        iny
        lda     (STRNG1),y
        beq     L32A9
        cmp     CHARAC
        beq     L32A5
        cmp     ENDCHR
        bne     L3298
L32A5:
        cmp     #$22
        beq     L32AA
L32A9:
        clc
L32AA:
        sty     FAC
        tya
        adc     STRNG1
        sta     STRNG2
        ldx     STRNG1+1
        bcc     L32B6
        inx
L32B6:
        stx     STRNG2+1
        lda     STRNG1+1
        bne     PUTNEW
        tya
        jsr     STRINI
        ldx     STRNG1
        ldy     STRNG1+1
        jsr     MOVSTR
; ----------------------------------------------------------------------------
; STORE DESCRIPTOR IN TEMPORARY DESCRIPTOR STACK
;
; THE DESCRIPTOR IS NOW IN FAC, FAC+1, FAC+2
; PUT ADDRESS OF TEMP DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
PUTNEW:
        ldx     TEMPPT
        cpx     #TEMPST+9
        bne     PUTEMP
        ldx     #ERR_FRMCPX
JERR:
        jmp     ERROR
PUTEMP:
        lda     FAC
        sta     0,x
        lda     FAC+1
        sta     1,x
        lda     FAC+2
        sta     2,x
        ldy     #$00
        stx     FAC_LAST-1
        sty     FAC_LAST
        dey
        sty     VALTYP
        stx     LASTPT
        inx
        inx
        inx
        stx     TEMPPT
        rts
; ----------------------------------------------------------------------------
; MAKE SPACE FOR STRING AT BOTTOM OF STRING SPACE
; (A)=# BYTES SPACE TO MAKE
;
; RETURN WITH (A) SAME,
;	AND Y,X = ADDRESS OF SPACE ALLOCATED
; ----------------------------------------------------------------------------
GETSPA:
        lsr     DATAFLG
L32F1:
        pha
        eor     #$FF
        sec
        adc     FRETOP
        ldy     FRETOP+1
        bcs     L32FC
        dey
L32FC:
        cpy     STREND+1
        bcc     L3311
        bne     L3306
        cmp     STREND
        bcc     L3311
L3306:
        sta     FRETOP
        sty     FRETOP+1
        sta     FRESPC
        sty     FRESPC+1
        tax
        pla
        rts
L3311:
        ldx     #ERR_MEMFULL
        lda     DATAFLG
        bmi     JERR
        jsr     GARBAG
        lda     #$80
        sta     DATAFLG
        pla
        bne     L32F1
; ----------------------------------------------------------------------------
; SHOVE ALL REFERENCED STRINGS AS HIGH AS POSSIBLE
; IN MEMORY (AGAINST HIMEM), FREEING UP SPACE
; BELOW STRING AREA DOWN TO STREND.
; ----------------------------------------------------------------------------
GARBAG:
        ldx     MEMSIZ
        lda     MEMSIZ+1
FINDHIGHESTSTRING:
        stx     FRETOP
        sta     FRETOP+1
        ldy     #$00
        sty     FNCNAM+1
        lda     STREND
        ldx     STREND+1
        sta     LOWTR
        stx     LOWTR+1
        lda     #TEMPST
        ldx     #$00
        sta     INDEX
        stx     INDEX+1
L333D:
        cmp     TEMPPT
        beq     L3346
        jsr     CHECK_VARIABLE
        beq     L333D
L3346:
        lda     #BYTES_PER_VARIABLE
        sta     DSCLEN
        lda     VARTAB
        ldx     VARTAB+1
        sta     INDEX
        stx     INDEX+1
L3352:
        cpx     ARYTAB+1
        bne     L335A
        cmp     ARYTAB
        beq     L335F
L335A:
        jsr     CHECK_SIMPLE_VARIABLE
        beq     L3352
L335F:
        sta     HIGHDS
        stx     HIGHDS+1
        lda     #$03	; OSI GC bugfix -> $04 ???
        sta     DSCLEN
L3367:
        lda     HIGHDS
        ldx     HIGHDS+1
L336B:
        cpx     STREND+1
        bne     L3376
        cmp     STREND
        bne     L3376
        jmp     MOVE_HIGHEST_STRING_TO_TOP
L3376:
        sta     INDEX
        stx     INDEX+1
        ldy     #$01
        lda     (INDEX),y
        php
        iny
        lda     (INDEX),y
        adc     HIGHDS
        sta     HIGHDS
        iny
        lda     (INDEX),y
        adc     HIGHDS+1
        sta     HIGHDS+1
        plp
        bpl     L3367
        iny
        lda     (INDEX),y
        asl     a
        adc     #$05
        adc     INDEX
        sta     INDEX
        bcc     L33A7
        inc     INDEX+1
L33A7:
        ldx     INDEX+1
L33A9:
        cpx     HIGHDS+1
        bne     L33B1
        cmp     HIGHDS
        beq     L336B
L33B1:
        jsr     CHECK_VARIABLE
        beq     L33A9
; ----------------------------------------------------------------------------
; PROCESS A SIMPLE VARIABLE
; ----------------------------------------------------------------------------
CHECK_SIMPLE_VARIABLE:
        iny
        lda     (INDEX),y
        bpl     CHECK_BUMP
        iny
; ----------------------------------------------------------------------------
; IF STRING IS NOT EMPTY, CHECK IF IT IS HIGHEST
; ----------------------------------------------------------------------------
CHECK_VARIABLE:
        lda     (INDEX),y
        beq     CHECK_BUMP
        iny
        lda     (INDEX),y
        tax
        iny
        lda     (INDEX),y
        cmp     FRETOP+1
        bcc     L33D5
        bne     CHECK_BUMP
        cpx     FRETOP
        bcs     CHECK_BUMP
L33D5:
        cmp     LOWTR+1
        bcc     CHECK_BUMP
        bne     L33DF
        cpx     LOWTR
        bcc     CHECK_BUMP
L33DF:
        stx     LOWTR
        sta     LOWTR+1
        lda     INDEX
        ldx     INDEX+1
        sta     FNCNAM
        stx     FNCNAM+1
        lda     DSCLEN
        sta     Z52
; ----------------------------------------------------------------------------
; ADD (DSCLEN) TO PNTR IN INDEX
; RETURN WITH Y=0, PNTR ALSO IN X,A
; ----------------------------------------------------------------------------
CHECK_BUMP:
        lda     DSCLEN
        clc
        adc     INDEX
        sta     INDEX
        bcc     L33FA
        inc     INDEX+1
L33FA:
        ldx     INDEX+1
        ldy     #$00
        rts
; ----------------------------------------------------------------------------
; FOUND HIGHEST NON-EMPTY STRING, SO MOVE IT
; TO TOP AND GO BACK FOR ANOTHER
; ----------------------------------------------------------------------------
MOVE_HIGHEST_STRING_TO_TOP:
        ldx     FNCNAM+1
        beq     L33FA
        lda     Z52
        and     #$04
        lsr     a
        tay
        sta     Z52
        lda     (FNCNAM),y
        adc     LOWTR
        sta     HIGHTR
        lda     LOWTR+1
        adc     #$00
        sta     HIGHTR+1
        lda     FRETOP
        ldx     FRETOP+1
        sta     HIGHDS
        stx     HIGHDS+1
        jsr     BLTU2
        ldy     Z52
        iny
        lda     HIGHDS
        sta     (FNCNAM),y
        tax
        inc     HIGHDS+1
        lda     HIGHDS+1
        iny
        sta     (FNCNAM),y
        jmp     FINDHIGHESTSTRING
; ----------------------------------------------------------------------------
; CONCATENATE TWO STRINGS
; ----------------------------------------------------------------------------
CAT:
        lda     FAC_LAST
        pha
        lda     FAC_LAST-1
        pha
        jsr     FRM_ELEMENT
        jsr     CHKSTR
        pla
        sta     STRNG1
        pla
        sta     STRNG1+1
        ldy     #$00
        lda     (STRNG1),y
        clc
        adc     (FAC_LAST-1),y
        bcc     L3454
        ldx     #ERR_STRLONG
        jmp     ERROR
L3454:
        jsr     STRINI
        jsr     MOVINS
        lda     DSCPTR
        ldy     DSCPTR+1
        jsr     FRETMP
        jsr     MOVSTR1
        lda     STRNG1
        ldy     STRNG1+1
        jsr     FRETMP
        jsr     PUTNEW
        jmp     FRMEVL2
; ----------------------------------------------------------------------------
; GET STRING DESCRIPTOR POINTED AT BY (STRNG1)
; AND MOVE DESCRIBED STRING TO (FRESPC)
; ----------------------------------------------------------------------------
MOVINS:
        ldy     #$00
        lda     (STRNG1),y
        pha
        iny
        lda     (STRNG1),y
        tax
        iny
        lda     (STRNG1),y
        tay
        pla
; ----------------------------------------------------------------------------
; MOVE STRING AT (Y,X) WITH LENGTH (A)
; TO DESTINATION WHOSE ADDRESS IS IN FRESPC,FRESPC+1
; ----------------------------------------------------------------------------
MOVSTR:
        stx     INDEX
        sty     INDEX+1
MOVSTR1:
        tay
        beq     L3490
        pha
L3487:
        dey
        lda     (INDEX),y
        sta     (FRESPC),y
        tya
        bne     L3487
        pla
L3490:
        clc
        adc     FRESPC
        sta     FRESPC
        bcc     L3499
        inc     FRESPC+1
L3499:
        rts
; ----------------------------------------------------------------------------
; IF (FAC) IS A TEMPORARY STRING, RELEASE DESCRIPTOR
; ----------------------------------------------------------------------------
FRESTR:
        jsr     CHKSTR
; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR POINTED TO BY FAC+3,4 IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FREFAC:
        lda     FAC_LAST-1
        ldy     FAC_LAST
; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR WHOSE ADDRESS IS IN Y,A IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FRETMP:
        sta     INDEX
        sty     INDEX+1
        jsr     FRETMS
        php
        ldy     #$00
        lda     (INDEX),y
        pha
        iny
        lda     (INDEX),y
        tax
        iny
        lda     (INDEX),y
        tay
        pla
        plp
        bne     L34CD
        cpy     FRETOP+1
        bne     L34CD
        cpx     FRETOP
        bne     L34CD
        pha
        clc
        adc     FRETOP
        sta     FRETOP
        bcc     L34CC
        inc     FRETOP+1
L34CC:
        pla
L34CD:
        stx     INDEX
        sty     INDEX+1
        rts
; ----------------------------------------------------------------------------
; RELEASE TEMPORARY DESCRIPTOR IF Y,A = LASTPT
; ----------------------------------------------------------------------------
FRETMS:
        cpy     LASTPT+1
        bne     L34E2
        cmp     LASTPT
        bne     L34E2
        sta     TEMPPT
        sbc     #$03
        sta     LASTPT
        ldy     #$00
L34E2:
        rts
; ----------------------------------------------------------------------------
; "CHR$" FUNCTION
; ----------------------------------------------------------------------------
CHRSTR:
        jsr     CONINT
        txa
        pha
        lda     #$01
        jsr     STRSPA
        pla
        ldy     #$00
        sta     (FAC+1),y
        pla
        pla
        jmp     PUTNEW
; ----------------------------------------------------------------------------
; "LEFT$" FUNCTION
; ----------------------------------------------------------------------------
LEFTSTR:
        jsr     SUBSTRING_SETUP
        cmp     (DSCPTR),y
        tya
SUBSTRING1:
        bcc     L3503
        lda     (DSCPTR),y
        tax
        tya
L3503:
        pha
SUBSTRING2:
        txa
SUBSTRING3:
        pha
        jsr     STRSPA
        lda     DSCPTR
        ldy     DSCPTR+1
        jsr     FRETMP
        pla
        tay
        pla
        clc
        adc     INDEX
        sta     INDEX
        bcc     L351C
        inc     INDEX+1
L351C:
        tya
        jsr     MOVSTR1
        jmp     PUTNEW
; ----------------------------------------------------------------------------
; "RIGHT$" FUNCTION
; ----------------------------------------------------------------------------
RIGHTSTR:
        jsr     SUBSTRING_SETUP
        clc
        sbc     (DSCPTR),y
        eor     #$FF
        jmp     SUBSTRING1
; ----------------------------------------------------------------------------
; "MID$" FUNCTION
; ----------------------------------------------------------------------------
MIDSTR:
        lda     #$FF
        sta     FAC_LAST
        jsr     CHRGOT
        cmp     #$29
        beq     L353F
        jsr     CHKCOM
        jsr     GETBYT
L353F:
        jsr     SUBSTRING_SETUP
        dex
        txa
        pha
        clc
        ldx     #$00
        sbc     (DSCPTR),y
        bcs     SUBSTRING2
        eor     #$FF
        cmp     FAC_LAST
        bcc     SUBSTRING3
        lda     FAC_LAST
        bcs     SUBSTRING3
; ----------------------------------------------------------------------------
; COMMON SETUP ROUTINE FOR LEFT$, RIGHT$, MID$:
; REQUIRE ")"; POP RETURN ADRS, GET DESCRIPTOR
; ADDRESS, GET 1ST PARAMETER OF COMMAND
; ----------------------------------------------------------------------------
SUBSTRING_SETUP:
        jsr     CHKCLS
        pla
        sta     JMPADRS+1
        pla
        sta     JMPADRS+2
        pla
        pla
        pla
        tax
        pla
        sta     DSCPTR
        pla
        sta     DSCPTR+1
        ldy     #$00
        txa
        beq     GOIQ
        inc     JMPADRS+1
        jmp     (JMPADRS+1)
; ----------------------------------------------------------------------------
; "LEN" FUNCTION
; ----------------------------------------------------------------------------
LEN:
        jsr     GETSTR
SNGFLT1:
        jmp     SNGFLT
; ----------------------------------------------------------------------------
; IF LAST RESULT IS A TEMPORARY STRING, FREE IT
; MAKE VALTYP NUMERIC, RETURN LENGTH IN Y-REG
; ----------------------------------------------------------------------------
GETSTR:
        jsr     FRESTR
        ldx     #$00
        stx     VALTYP
        tay
        rts
; ----------------------------------------------------------------------------
; "ASC" FUNCTION
; ----------------------------------------------------------------------------
ASC:
        jsr     GETSTR
        beq     GOIQ
        ldy     #$00
        lda     (INDEX),y
        tay
        jmp     SNGFLT1
; ----------------------------------------------------------------------------
GOIQ:
        jmp     IQERR
; ----------------------------------------------------------------------------
; SCAN TO NEXT CHARACTER AND CONVERT EXPRESSION
; TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GTBYTC:
        jsr     CHRGET
; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION AT TXTPTR, AND
; CONVERT IT TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GETBYT:
        jsr     FRMNUM
; ----------------------------------------------------------------------------
; CONVERT (FAC) TO SINGLE BYTE INTEGER IN X-REG
; ----------------------------------------------------------------------------
CONINT:
        jsr     MKINT
        ldx     FAC_LAST-1
        bne     GOIQ
        ldx     FAC_LAST
        jmp     CHRGOT
; ----------------------------------------------------------------------------
; "VAL" FUNCTION
; ----------------------------------------------------------------------------
VAL:
        jsr     GETSTR
        bne     L35AC
        jmp     ZERO_FAC
L35AC:
        ldx     TXTPTR
        ldy     TXTPTR+1
        stx     STRNG2
        sty     STRNG2+1
        ldx     INDEX
        stx     TXTPTR
        clc
        adc     INDEX
        sta     DEST
        ldx     INDEX+1
        stx     TXTPTR+1
        bcc     L35C4
        inx
L35C4:
        stx     DEST+1
        ldy     #$00
        lda     (DEST),y
        pha
        lda     #$00
        sta     (DEST),y
        jsr     CHRGOT
        jsr     FIN
        pla
        ldy     #$00
        sta     (DEST),y
; ----------------------------------------------------------------------------
; COPY STRNG2 INTO TXTPTR
; ----------------------------------------------------------------------------
POINT:
        ldx     STRNG2
        ldy     STRNG2+1
        stx     TXTPTR
        sty     TXTPTR+1
        rts
; ----------------------------------------------------------------------------
; EVALUATE "EXP1,EXP2"
;
; CONVERT EXP1 TO 16-BIT NUMBER IN LINNUM
; CONVERT EXP2 TO 8-BIT NUMBER IN X-REG
; ----------------------------------------------------------------------------
GTNUM:
        jsr     FRMNUM
        jsr     GETADR
; ----------------------------------------------------------------------------
; EVALUATE ",EXPRESSION"
; CONVERT EXPRESSION TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
COMBYTE:
        jsr     CHKCOM
        jmp     GETBYT
; ----------------------------------------------------------------------------
; CONVERT (FAC) TO A 16-BIT VALUE IN LINNUM
; ----------------------------------------------------------------------------
GETADR:
        lda     FACSIGN
        bmi     GOIQ
        lda     FAC
        cmp     #$91
        bcs     GOIQ
        jsr     QINT
        lda     FAC_LAST-1
        ldy     FAC_LAST
        sty     LINNUM
        sta     LINNUM+1
        rts
; ----------------------------------------------------------------------------
; "PEEK" FUNCTION
; ----------------------------------------------------------------------------
PEEK:
        jsr     GETADR
        ldy     #$00
; disallow PEEK between $C000 and $DFFF
        lda     (LINNUM),y
        tay
        jmp     SNGFLT
; ----------------------------------------------------------------------------
; "POKE" STATEMENT
; ----------------------------------------------------------------------------
POKE:
        jsr     GTNUM
        txa
        ldy     #$00
        sta     (LINNUM),y
        rts
; ----------------------------------------------------------------------------
; "WAIT" STATEMENT
; ----------------------------------------------------------------------------
WAIT:
        jsr     GTNUM
        stx     FORPNT
        ldx     #$00
        jsr     CHRGOT
        beq     L3628
        jsr     COMBYTE
L3628:
        stx     FORPNT+1
        ldy     #$00
L362C:
        lda     (LINNUM),y
        eor     FORPNT+1
        and     FORPNT
        beq     L362C
RTS3:
        rts
TEMP1X = TEMP1+(5-BYTES_FP)
; ----------------------------------------------------------------------------
; ADD 0.5 TO FAC
; ----------------------------------------------------------------------------
FADDH:
        lda     #lo(CON_HALF)
        ldy     #hi(CON_HALF)
        jmp     FADD
; ----------------------------------------------------------------------------
; FAC = (Y,A) - FAC
; ----------------------------------------------------------------------------
FSUB:
        jsr     LOAD_ARG_FROM_YA
; ----------------------------------------------------------------------------
; FAC = ARG - FAC
; ----------------------------------------------------------------------------
FSUBT:
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
        eor     ARGSIGN
        sta     SGNCPR
        lda     FAC
        jmp     FADDT
; ----------------------------------------------------------------------------
; Commodore BASIC V2 Easter Egg
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; SHIFT SMALLER ARGUMENT MORE THAN 7 BITS
; ----------------------------------------------------------------------------
FADD1:
        jsr     SHIFT_RIGHT
        bcc     FADD3
; ----------------------------------------------------------------------------
; FAC = (Y,A) + FAC
; ----------------------------------------------------------------------------
FADD:
        jsr     LOAD_ARG_FROM_YA
; ----------------------------------------------------------------------------
; FAC = ARG + FAC
; ----------------------------------------------------------------------------
FADDT:
        bne     L365B
        jmp     COPY_ARG_TO_FAC
L365B:
        ldx     FACEXTENSION
        stx     ARGEXTENSION
        ldx     #ARG
        lda     ARG
FADD2:
        tay
        beq     RTS3
        sec
        sbc     FAC
        beq     FADD3
        bcc     L367F
        sty     FAC
        ldy     ARGSIGN
        sty     FACSIGN
        eor     #$FF
        adc     #$00
        ldy     #$00
        sty     ARGEXTENSION
        ldx     #FAC
        bne     L3683
L367F:
        ldy     #$00
        sty     FACEXTENSION
L3683:
        cmp     #$F9
        bmi     FADD1
        tay
        lda     FACEXTENSION
        lsr     1,x
        jsr     SHIFT_RIGHT4
FADD3:
        bit     SGNCPR
        bpl     FADD4
        ldy     #FAC
        cpx     #ARG
        beq     L369B
        ldy     #ARG
L369B:
        sec
        eor     #$FF
        adc     ARGEXTENSION
        sta     FACEXTENSION
        lda     3,y
        sbc     3,x
        sta     FAC+3
        lda     2,y
        sbc     2,x
        sta     FAC+2
        lda     1,y
        sbc     1,x
        sta     FAC+1
; ----------------------------------------------------------------------------
; NORMALIZE VALUE IN FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC1:
        bcs     NORMALIZE_FAC2
        jsr     COMPLEMENT_FAC
NORMALIZE_FAC2:
        ldy     #$00
        tya
        clc
L36C7:
        ldx     FAC+1
        bne     NORMALIZE_FAC4
        ldx     FAC+2
        stx     FAC+1
        ldx     FAC+3
        stx     FAC+2
        ldx     FACEXTENSION
        stx     FAC+3
        sty     FACEXTENSION
        adc     #$08
; bugfix?
; fix does not exist on AppleSoft 2
        cmp     #MANTISSA_BYTES*8
        bne     L36C7
; ----------------------------------------------------------------------------
; SET FAC = 0
; (ONLY NECESSARY TO ZERO EXPONENT AND SIGN CELLS)
; ----------------------------------------------------------------------------
ZERO_FAC:
        lda     #$00
STA_IN_FAC_SIGN_AND_EXP:
        sta     FAC
STA_IN_FAC_SIGN:
        sta     FACSIGN
        rts
; ----------------------------------------------------------------------------
; ADD MANTISSAS OF FAC AND ARG INTO FAC
; ----------------------------------------------------------------------------
FADD4:
        adc     ARGEXTENSION
        sta     FACEXTENSION
        lda     FAC+3
        adc     ARG+3
        sta     FAC+3
        lda     FAC+2
        adc     ARG+2
        sta     FAC+2
        lda     FAC+1
        adc     ARG+1
        sta     FAC+1
        jmp     NORMALIZE_FAC5
; ----------------------------------------------------------------------------
; FINISH NORMALIZING FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC3:
        adc     #$01
        asl     FACEXTENSION
        rol     FAC+3
        rol     FAC+2
        rol     FAC+1
NORMALIZE_FAC4:
        bpl     NORMALIZE_FAC3
        sec
        sbc     FAC
        bcs     ZERO_FAC
        eor     #$FF
        adc     #$01
        sta     FAC
NORMALIZE_FAC5:
        bcc     L3764
NORMALIZE_FAC6:
        inc     FAC
        beq     OVERFLOW
        ror     FAC+1
        ror     FAC+2
        ror     FAC+3
        ror     FACEXTENSION
L3764:
        rts
; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC
; ----------------------------------------------------------------------------
COMPLEMENT_FAC:
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC MANTISSA ONLY
; ----------------------------------------------------------------------------
COMPLEMENT_FAC_MANTISSA:
        lda     FAC+1
        eor     #$FF
        sta     FAC+1
        lda     FAC+2
        eor     #$FF
        sta     FAC+2
        lda     FAC+3
        eor     #$FF
        sta     FAC+3
        lda     FACEXTENSION
        eor     #$FF
        sta     FACEXTENSION
        inc     FACEXTENSION
        bne     RTS12
; ----------------------------------------------------------------------------
; INCREMENT FAC MANTISSA
; ----------------------------------------------------------------------------
INCREMENT_FAC_MANTISSA:
        inc     FAC+3
        bne     RTS12
        inc     FAC+2
        bne     RTS12
        inc     FAC+1
RTS12:
        rts
OVERFLOW:
        ldx     #ERR_OVERFLOW
        jmp     ERROR
; ----------------------------------------------------------------------------
; SHIFT 1,X THRU 5,X RIGHT
; (A) = NEGATIVE OF SHIFT COUNT
; (X) = POINTER TO BYTES TO BE SHIFTED
;
; RETURN WITH (Y)=0, CARRY=0, EXTENSION BITS IN A-REG
; ----------------------------------------------------------------------------
SHIFT_RIGHT1:
        ldx     #RESULT-1
SHIFT_RIGHT2:
        ldy     3,x
        sty     FACEXTENSION
        ldy     2,x
        sty     3,x
        ldy     1,x
        sty     2,x
        ldy     SHIFTSIGNEXT
        sty     1,x
; ----------------------------------------------------------------------------
; MAIN ENTRY TO RIGHT SHIFT SUBROUTINE
; ----------------------------------------------------------------------------
SHIFT_RIGHT:
        adc     #$08
        bmi     SHIFT_RIGHT2
        beq     SHIFT_RIGHT2
        sbc     #$08
        tay
        lda     FACEXTENSION
        bcs     SHIFT_RIGHT5
LB588:
        asl     1,x
        bcc     LB58E
        inc     1,x
LB58E:
        ror     1,x
        ror     1,x
; ----------------------------------------------------------------------------
; ENTER HERE FOR SHORT SHIFTS WITH NO SIGN EXTENSION
; ----------------------------------------------------------------------------
SHIFT_RIGHT4:
        ror     2,x
        ror     3,x
        ror     a
        iny
        bne     LB588
SHIFT_RIGHT5:
        clc
        rts
; ----------------------------------------------------------------------------
CON_ONE:
        db   $81,$00,$00,$00
POLY_LOG:
        db	$02
		  db   $80,$19,$56,$62
		  db   $80,$76,$22,$F3
		  db   $82,$38,$AA,$40
CON_SQR_HALF:
		  db   $80,$35,$04,$F3
CON_SQR_TWO:
		  db   $81,$35,$04,$F3
CON_NEG_HALF:
		  db   $80,$80,$00,$00
CON_LOG_TWO:
		  db   $80,$31,$72,$18
; ----------------------------------------------------------------------------
; "LOG" FUNCTION
; ----------------------------------------------------------------------------
LOG:
        jsr     SIGN
        beq     GIQ
        bpl     LOG2
GIQ:
        jmp     IQERR
LOG2:
        lda     FAC
        sbc     #$7F
        pha
        lda     #$80
        sta     FAC
        lda     #lo(CON_SQR_HALF)
        ldy     #hi(CON_SQR_HALF)
        jsr     FADD
        lda     #lo(CON_SQR_TWO)
        ldy     #hi(CON_SQR_TWO)
        jsr     FDIV
        lda     #lo(CON_ONE)
        ldy     #hi(CON_ONE)
        jsr     FSUB
        lda     #lo(POLY_LOG)
        ldy     #hi(POLY_LOG)
        jsr     POLYNOMIAL_ODD
        lda     #lo(CON_NEG_HALF)
        ldy     #hi(CON_NEG_HALF)
        jsr     FADD
        pla
        jsr     ADDACC
        lda     #lo(CON_LOG_TWO)
        ldy     #hi(CON_LOG_TWO)
; ----------------------------------------------------------------------------
; FAC = (Y,A) * FAC
; ----------------------------------------------------------------------------
FMULT:
        jsr     LOAD_ARG_FROM_YA
; ----------------------------------------------------------------------------
; FAC = ARG * FAC
; ----------------------------------------------------------------------------
FMULTT:
        beq     L3903
        jsr     ADD_EXPONENTS
        lda     #$00
        sta     RESULT
        sta     RESULT+1
        sta     RESULT+2
        lda     FACEXTENSION
        jsr     MULTIPLY1
        lda     FAC+3
        jsr     MULTIPLY1
        lda     FAC+2
        jsr     MULTIPLY1
        lda     FAC+1
        jsr     MULTIPLY2
        jmp     COPY_RESULT_INTO_FAC
; ----------------------------------------------------------------------------
; MULTIPLY ARG BY (A) INTO RESULT
; ----------------------------------------------------------------------------
MULTIPLY1:
        bne     MULTIPLY2
        jmp     SHIFT_RIGHT1
MULTIPLY2:
        lsr     a
        ora     #$80
L38A7:
        tay
        bcc     L38C3
        clc
        lda     RESULT+2
        adc     ARG+3
        sta     RESULT+2
        lda     RESULT+1
        adc     ARG+2
        sta     RESULT+1
        lda     RESULT
        adc     ARG+1
        sta     RESULT
L38C3:
        ror     RESULT
        ror     RESULT+1
; this seems to be a bad byte in the dump
        ror     RESULT+2
        ror     FACEXTENSION
        tya
        lsr     a
        bne     L38A7
L3903:
        rts
; ----------------------------------------------------------------------------
; UNPACK NUMBER AT (Y,A) INTO ARG
; ----------------------------------------------------------------------------
LOAD_ARG_FROM_YA:
        sta     INDEX
        sty     INDEX+1
        ldy     #BYTES_FP-1
        lda     (INDEX),y
        sta     ARG+3
        dey
        lda     (INDEX),y
        sta     ARG+2
        dey
        lda     (INDEX),y
        sta     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
        lda     ARGSIGN
        ora     #$80
        sta     ARG+1
        dey
        lda     (INDEX),y
        sta     ARG
        lda     FAC
        rts
; ----------------------------------------------------------------------------
; ADD EXPONENTS OF ARG AND FAC
; (CALLED BY FMULT AND FDIV)
;
; ALSO CHECK FOR OVERFLOW, AND SET RESULT SIGN
; ----------------------------------------------------------------------------
ADD_EXPONENTS:
        lda     ARG
ADD_EXPONENTS1:
        beq     ZERO
        clc
        adc     FAC
        bcc     L393C
        bmi     JOV
        clc
        db   $2C
L393C:
        bpl     ZERO
        adc     #$80
        sta     FAC
        bne     L3947
        jmp     STA_IN_FAC_SIGN
L3947:
        lda     SGNCPR
        sta     FACSIGN
        rts
; ----------------------------------------------------------------------------
; IF (FAC) IS POSITIVE, GIVE "OVERFLOW" ERROR
; IF (FAC) IS NEGATIVE, SET FAC=0, POP ONE RETURN, AND RTS
; CALLED FROM "EXP" FUNCTION
; ----------------------------------------------------------------------------
OUTOFRNG:
        lda     FACSIGN
        eor     #$FF
        bmi     JOV
; ----------------------------------------------------------------------------
; POP RETURN ADDRESS AND SET FAC=0
; ----------------------------------------------------------------------------
ZERO:
        pla
        pla
        jmp     ZERO_FAC
JOV:
        jmp     OVERFLOW
; ----------------------------------------------------------------------------
; MULTIPLY FAC BY 10
; ----------------------------------------------------------------------------
MUL10:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        tax
        beq     L3970
        clc
        adc     #$02
        bcs     JOV
        ldx     #$00
        stx     SGNCPR
        jsr     FADD2
        inc     FAC
        beq     JOV
L3970:
        rts
; ----------------------------------------------------------------------------
CONTEN:
        db   $84,$20,$00,$00
; ----------------------------------------------------------------------------
; DIVIDE FAC BY 10
; ----------------------------------------------------------------------------
DIV10:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #lo(CONTEN)
        ldy     #hi(CONTEN)
        ldx     #$00
; ----------------------------------------------------------------------------
; FAC = ARG / (Y,A)
; ----------------------------------------------------------------------------
DIV:
        stx     SGNCPR
        jsr     LOAD_FAC_FROM_YA
        jmp     FDIVT
; ----------------------------------------------------------------------------
; FAC = (Y,A) / FAC
; ----------------------------------------------------------------------------
FDIV:
        jsr     LOAD_ARG_FROM_YA
; ----------------------------------------------------------------------------
; FAC = ARG / FAC
; ----------------------------------------------------------------------------
FDIVT:
        beq     L3A02
        jsr     ROUND_FAC
        lda     #$00
        sec
        sbc     FAC
        sta     FAC
        jsr     ADD_EXPONENTS
        inc     FAC
        beq     JOV
        ldx     #-MANTISSA_BYTES
        lda     #$01
L39A1:
        ldy     ARG+1
        cpy     FAC+1
        bne     L39B7
        ldy     ARG+2
        cpy     FAC+2
        bne     L39B7
        ldy     ARG+3
        cpy     FAC+3
L39B7:
        php
        rol     a
        bcc     L39C4
        inx
        sta     RESULT_LAST-1,x
        beq     L39F2
        bpl     L39F6
        lda     #$01
L39C4:
        plp
        bcs     L39D5
L39C7:
        asl     ARG_LAST
        rol     ARG+2
        rol     ARG+1
        bcs     L39B7
        bmi     L39A1
        bpl     L39B7
L39D5:
        tay
        lda     ARG+3
        sbc     FAC+3
        sta     ARG+3
        lda     ARG+2
        sbc     FAC+2
        sta     ARG+2
        lda     ARG+1
        sbc     FAC+1
        sta     ARG+1
        tya
        jmp     L39C7
L39F2:
        lda     #$40
        bne     L39C4
L39F6:
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     FACEXTENSION
        plp
        jmp     COPY_RESULT_INTO_FAC
L3A02:
        ldx     #ERR_ZERODIV
        jmp     ERROR
; ----------------------------------------------------------------------------
; COPY RESULT INTO FAC MANTISSA, AND NORMALIZE
; ----------------------------------------------------------------------------
COPY_RESULT_INTO_FAC:
        lda     RESULT
        sta     FAC+1
        lda     RESULT+1
        sta     FAC+2
        lda     RESULT+2
        sta     FAC+3
        jmp     NORMALIZE_FAC2
; ----------------------------------------------------------------------------
; UNPACK (Y,A) INTO FAC
; ----------------------------------------------------------------------------
LOAD_FAC_FROM_YA:
        sta     INDEX
        sty     INDEX+1
        ldy     #MANTISSA_BYTES
        lda     (INDEX),y
        sta     FAC+3
        dey
        lda     (INDEX),y
        sta     FAC+2
        dey
        lda     (INDEX),y
        sta     FACSIGN
        ora     #$80
        sta     FAC+1
        dey
        lda     (INDEX),y
        sta     FAC
        sty     FACEXTENSION
        rts
; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP2
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP2_ROUNDED:
        ldx     #TEMP2
        db   $2C
; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP1
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP1_ROUNDED:
        ldx     #TEMP1X
        ldy     #$00
        beq     STORE_FAC_AT_YX_ROUNDED
; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE WHERE FORPNT POINTS
; ----------------------------------------------------------------------------
SETFOR:
        ldx     FORPNT
        ldy     FORPNT+1
; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE AT (Y,X)
; ----------------------------------------------------------------------------
STORE_FAC_AT_YX_ROUNDED:
        jsr     ROUND_FAC
        stx     INDEX
        sty     INDEX+1
        ldy     #MANTISSA_BYTES
        lda     FAC+3
        sta     (INDEX),y
        dey
        lda     FAC+2
        sta     (INDEX),y
        dey
        lda     FACSIGN
        ora     #$7F
        and     FAC+1
        sta     (INDEX),y
        dey
        lda     FAC
        sta     (INDEX),y
        sty     FACEXTENSION
        rts
; ----------------------------------------------------------------------------
; COPY ARG INTO FAC
; ----------------------------------------------------------------------------
COPY_ARG_TO_FAC:
        lda     ARGSIGN
MFA:
        sta     FACSIGN
        ldx     #BYTES_FP
L3A7A:
        lda     SHIFTSIGNEXT,x
        sta     EXPSGN,x
        dex
        bne     L3A7A
        stx     FACEXTENSION
        rts
; ----------------------------------------------------------------------------
; ROUND FAC AND COPY TO ARG
; ----------------------------------------------------------------------------
COPY_FAC_TO_ARG_ROUNDED:
        jsr     ROUND_FAC
MAF:
        ldx     #BYTES_FP+1
L3A89:
        lda     EXPSGN,x
        sta     SHIFTSIGNEXT,x
        dex
        bne     L3A89
        stx     FACEXTENSION
RTS14:
        rts
; ----------------------------------------------------------------------------
; ROUND FAC USING EXTENSION BYTE
; ----------------------------------------------------------------------------
ROUND_FAC:
        lda     FAC
        beq     RTS14
        asl     FACEXTENSION
        bcc     RTS14
; ----------------------------------------------------------------------------
; INCREMENT MANTISSA AND RE-NORMALIZE IF CARRY
; ----------------------------------------------------------------------------
INCREMENT_MANTISSA:
        jsr     INCREMENT_FAC_MANTISSA
        bne     RTS14
        jmp     NORMALIZE_FAC6
; ----------------------------------------------------------------------------
; TEST FAC FOR ZERO AND SIGN
;
; FAC > 0, RETURN +1
; FAC = 0, RETURN  0
; FAC < 0, RETURN -1
; ----------------------------------------------------------------------------
SIGN:
        lda     FAC
        beq     RTS15
L3AA7:
        lda     FACSIGN
SIGN2:
        rol     a
        lda     #$FF
        bcs     RTS15
        lda     #$01
RTS15:
        rts
; ----------------------------------------------------------------------------
; "SGN" FUNCTION
; ----------------------------------------------------------------------------
SGN:
        jsr     SIGN
; ----------------------------------------------------------------------------
; CONVERT (A) INTO FAC, AS SIGNED VALUE -128 TO +127
; ----------------------------------------------------------------------------
FLOAT:
        sta     FAC+1
        lda     #$00
        sta     FAC+2
        ldx     #$88
; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; ----------------------------------------------------------------------------
FLOAT1:
        lda     FAC+1
        eor     #$FF
        rol     a
; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; C=0 TO MAKE VALUE NEGATIVE
; C=1 TO MAKE VALUE POSITIVE
; ----------------------------------------------------------------------------
FLOAT2:
        lda     #$00
        sta     FAC+3
        stx     FAC
        sta     FACEXTENSION
        sta     FACSIGN
        jmp     NORMALIZE_FAC1
; ----------------------------------------------------------------------------
; "ABS" FUNCTION
; ----------------------------------------------------------------------------
ABS:
        lsr     FACSIGN
        rts
; ----------------------------------------------------------------------------
; COMPARE FAC WITH PACKED # AT (Y,A)
; RETURN A=1,0,-1 AS (Y,A) IS <,=,> FAC
; ----------------------------------------------------------------------------
FCOMP:
        sta     DEST
; ----------------------------------------------------------------------------
; SPECIAL ENTRY FROM "NEXT" PROCESSOR
; "DEST" ALREADY SET UP
; ----------------------------------------------------------------------------
FCOMP2:
        sty     DEST+1
        ldy     #$00
        lda     (DEST),y
        iny
        tax
        beq     SIGN
        lda     (DEST),y
        eor     FACSIGN
        bmi     L3AA7
        cpx     FAC
        bne     L3B0A
        lda     (DEST),y
        ora     #$80
        cmp     FAC+1
        bne     L3B0A
        iny
        lda     (DEST),y
        cmp     FAC+2
        bne     L3B0A
        iny
        lda     #$7F
        cmp     FACEXTENSION
        lda     (DEST),y
        sbc     FAC_LAST
        beq     L3B32
L3B0A:
        lda     FACSIGN
        bcc     L3B10
        eor     #$FF
L3B10:
        jmp     SIGN2
; ----------------------------------------------------------------------------
; QUICK INTEGER FUNCTION
;
; CONVERTS FP VALUE IN FAC TO INTEGER VALUE
; IN FAC+1...FAC+4, BY SHIFTING RIGHT WITH SIGN
; EXTENSION UNTIL FRACTIONAL BITS ARE OUT.
;
; THIS SUBROUTINE ASSUMES THE EXPONENT < 32.
; ----------------------------------------------------------------------------
QINT:
        lda     FAC
        beq     QINT3
        sec
        sbc     #120+8*BYTES_FP
        bit     FACSIGN
        bpl     L3B27
        tax
        lda     #$FF
        sta     SHIFTSIGNEXT
        jsr     COMPLEMENT_FAC_MANTISSA
        txa
L3B27:
        ldx     #FAC
        cmp     #$F9
        bpl     QINT2
        jsr     SHIFT_RIGHT
        sty     SHIFTSIGNEXT
L3B32:
        rts
QINT2:
        tay
        lda     FACSIGN
        and     #$80
        lsr     FAC+1
        ora     FAC+1
        sta     FAC+1
        jsr     SHIFT_RIGHT4
        sty     SHIFTSIGNEXT
        rts
; ----------------------------------------------------------------------------
; "INT" FUNCTION
;
; USES QINT TO CONVERT (FAC) TO INTEGER FORM,
; AND THEN REFLOATS THE INTEGER.
; ----------------------------------------------------------------------------
INT:
        lda     FAC
        cmp     #120+8*BYTES_FP
        bcs     RTS17
        jsr     QINT
        sty     FACEXTENSION
        lda     FACSIGN
        sty     FACSIGN
        eor     #$80
        rol     a
        lda     #120+8*BYTES_FP
        sta     FAC
        lda     FAC_LAST
        sta     CHARAC
        jmp     NORMALIZE_FAC1
QINT3:
        sta     FAC+1
        sta     FAC+2
        sta     FAC+3
        tay
RTS17:
        rts
; ----------------------------------------------------------------------------
; CONVERT STRING TO FP VALUE IN FAC
;
; STRING POINTED TO BY TXTPTR
; FIRST CHAR ALREADY SCANNED BY CHRGET
; (A) = FIRST CHAR, C=0 IF DIGIT.
; ----------------------------------------------------------------------------
FIN:
        ldy     #$00
        ldx     #SERLEN-TMPEXP
L3B6F:
        sty     TMPEXP,x
        dex
        bpl     L3B6F
        bcc     FIN2
        cmp     #$2D
        bne     L3B7E
        stx     SERLEN
        beq     FIN1
L3B7E:
        cmp     #$2B
        bne     FIN3
FIN1:
        jsr     CHRGET
FIN2:
        bcc     FIN9
FIN3:
        cmp     #$2E
        beq     FIN10
        cmp     #$45
        bne     FIN7
        jsr     CHRGET
        bcc     FIN5
        cmp     #TOKEN_MINUS
        beq     L3BA6
        cmp     #$2D
        beq     L3BA6
        cmp     #TOKEN_PLUS
        beq     FIN4
        cmp     #$2B
        beq     FIN4
        bne     FIN6
L3BA6:
        ror     EXPSGN
FIN4:
        jsr     CHRGET
FIN5:
        bcc     GETEXP
FIN6:
        bit     EXPSGN
        bpl     FIN7
        lda     #$00
        sec
        sbc     EXPON
        jmp     FIN8
; ----------------------------------------------------------------------------
; FOUND A DECIMAL POINT
; ----------------------------------------------------------------------------
FIN10:
        ror     LOWTR
        bit     LOWTR
        bvc     FIN1
; ----------------------------------------------------------------------------
; NUMBER TERMINATED, ADJUST EXPONENT NOW
; ----------------------------------------------------------------------------
FIN7:
        lda     EXPON
FIN8:
        sec
        sbc     INDX
        sta     EXPON
        beq     L3BEE
        bpl     L3BE7
L3BDE:
        jsr     DIV10
        inc     EXPON
        bne     L3BDE
        beq     L3BEE
L3BE7:
        jsr     MUL10
        dec     EXPON
        bne     L3BE7
L3BEE:
        lda     SERLEN
        bmi     L3BF3
        rts
L3BF3:
        jmp     NEGOP
; ----------------------------------------------------------------------------
; ACCUMULATE A DIGIT INTO FAC
; ----------------------------------------------------------------------------
FIN9:
        pha
        bit     LOWTR
        bpl     L3BFD
        inc     INDX
L3BFD:
        jsr     MUL10
        pla
        sec
        sbc     #$30
        jsr     ADDACC
        jmp     FIN1
; ----------------------------------------------------------------------------
; ADD (A) TO FAC
; ----------------------------------------------------------------------------
ADDACC:
        pha
        jsr     COPY_FAC_TO_ARG_ROUNDED
        pla
        jsr     FLOAT
        lda     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
        ldx     FAC
        jmp     FADDT
; ----------------------------------------------------------------------------
; ACCUMULATE DIGIT OF EXPONENT
; ----------------------------------------------------------------------------
GETEXP:
        lda     EXPON
        cmp     #MAX_EXPON
        bcc     L3C2C
        lda     #$64
        bit     EXPSGN
        bmi     L3C3A
        jmp     OVERFLOW
L3C2C:
        asl     a
        asl     a
        clc
        adc     EXPON
        asl     a
        clc
        ldy     #$00
        adc     (TXTPTR),y
        sec
        sbc     #$30
L3C3A:
        sta     EXPON
        jmp     FIN4
; ----------------------------------------------------------------------------
; these values are /1000 of what the labels say
CON_99999999_9:
        db   $91,$43,$4F,$F8
CON_999999999:
		  db   $94,$74,$23,$F7
CON_BILLION:
        db   $94,$74,$24,$00
; ----------------------------------------------------------------------------
; PRINT "IN <LINE #hi("
; ----------------------------------------------------------------------------
INPRT:
        lda     #lo(QT_IN)
        ldy     #hi(QT_IN)
        jsr     GOSTROUT2
        lda     CURLIN+1
        ldx     CURLIN
; ----------------------------------------------------------------------------
; PRINT A,X AS DECIMAL INTEGER
; ----------------------------------------------------------------------------
LINPRT:
        sta     FAC+1
        stx     FAC+2
        ldx     #$90
        sec
        jsr     FLOAT2
        jsr     FOUT
GOSTROUT2:
        jmp     STROUT
; ----------------------------------------------------------------------------
; CONVERT (FAC) TO STRING STARTING AT STACK
; RETURN WITH (Y,A) POINTING AT STRING
; ----------------------------------------------------------------------------
FOUT:
        ldy     #$01
; ----------------------------------------------------------------------------
; "STR$" FUNCTION ENTERS HERE, WITH (Y)=0
; SO THAT RESULT STRING STARTS AT STACK-1
; (THIS IS USED AS A FLAG)
; ----------------------------------------------------------------------------
FOUT1:
        lda     #$20
        bit     FACSIGN
        bpl     L3C73
        lda     #$2D
L3C73:
        sta     $FF,y
        sta     FACSIGN
        sty     STRNG2
        iny
        lda     #$30
        ldx     FAC
        bne     L3C84
        jmp     FOUT4
L3C84:
        lda     #$00
        cpx     #$80
        beq     L3C8C
        bcs     L3C95
L3C8C:
        lda     #lo(CON_BILLION)
        ldy     #hi(CON_BILLION)
        jsr     FMULT
        lda     #-6 ; exponent adjustment
L3C95:
        sta     INDX
; ----------------------------------------------------------------------------
; ADJUST UNTIL 1E8 <= (FAC) <1E9
; ----------------------------------------------------------------------------
L3C97:
        lda     #lo(CON_999999999)
        ldy     #hi(CON_999999999)
        jsr     FCOMP
        beq     L3CBE
        bpl     L3CB4
L3CA2:
        lda     #lo(CON_99999999_9)
        ldy     #hi(CON_99999999_9)
        jsr     FCOMP
        beq     L3CAD
        bpl     L3CBB
L3CAD:
        jsr     MUL10
        dec     INDX
        bne     L3CA2
L3CB4:
        jsr     DIV10
        inc     INDX
        bne     L3C97
L3CBB:
        jsr     FADDH
L3CBE:
        jsr     QINT
; ----------------------------------------------------------------------------
; FAC+1...FAC+4 IS NOW IN INTEGER FORM
; WITH POWER OF TEN ADJUSTMENT IN TMPEXP
;
; IF -10 < TMPEXP > 1, PRINT IN DECIMAL FORM
; OTHERWISE, PRINT IN EXPONENTIAL FORM
; ----------------------------------------------------------------------------
        ldx     #$01
        lda     INDX
        clc
        adc     #3*BYTES_FP-5
        bmi     L3CD3
        cmp     #3*BYTES_FP-4
        bcs     L3CD4
        adc     #$FF
        tax
        lda     #$02
L3CD3:
        sec
L3CD4:
        sbc     #$02
        sta     EXPON
        stx     INDX
        txa
        beq     L3CDF
        bpl     L3CF2
L3CDF:
        ldy     STRNG2
        lda     #$2E
        iny
        sta     $FF,y
        txa
        beq     L3CF0
        lda     #$30
        iny
        sta     $FF,y
L3CF0:
        sty     STRNG2
; ----------------------------------------------------------------------------
; NOW DIVIDE BY POWERS OF TEN TO GET SUCCESSIVE DIGITS
; ----------------------------------------------------------------------------
L3CF2:
        ldy     #$00
        ldx     #$80
L3CF6:
        lda     FAC_LAST
        clc
        adc     DECTBL+2,y
        sta     FAC+3
        lda     FAC+2
        adc     DECTBL+1,y
        sta     FAC+2
        lda     FAC+1
        adc     DECTBL,y
        sta     FAC+1
        inx
        bcs     L3D1A
        bpl     L3CF6
        bmi     L3D1C
L3D1A:
        bmi     L3CF6
L3D1C:
        txa
        bcc     L3D23
        eor     #$FF
        adc     #$0A
L3D23:
        adc     #$2F
        iny
        iny
        iny
        sty     VARPNT
        ldy     STRNG2
        iny
        tax
        and     #$7F
        sta     $FF,y
        dec     INDX
        bne     L3D3E
        lda     #$2E
        iny
        sta     $FF,y
L3D3E:
        sty     STRNG2
        ldy     VARPNT
        txa
        eor     #$FF
        and     #$80
        tax
        cpy     #DECTBL_END-DECTBL
        bne     L3CF6
; ----------------------------------------------------------------------------
; NINE DIGITS HAVE BEEN STORED IN STRING.  NOW LOOK
; BACK AND LOP OFF TRAILING ZEROES AND A TRAILING
; DECIMAL POINT.
; ----------------------------------------------------------------------------
        ldy     STRNG2
L3D4E:
        lda     $FF,y
        dey
        cmp     #$30
        beq     L3D4E
        cmp     #$2E
        beq     L3D5B
        iny
L3D5B:
        lda     #$2B
        ldx     EXPON
        beq     L3D8F
        bpl     L3D6B
        lda     #$00
        sec
        sbc     EXPON
        tax
        lda     #$2D
L3D6B:
        sta     STACK+1,y
        lda     #$45
        sta     STACK,y
        txa
        ldx     #$2F
        sec
L3D77:
        inx
        sbc     #$0A
        bcs     L3D77
        adc     #$3A
        sta     STACK+3,y
        txa
        sta     STACK+2,y
        lda     #$00
        sta     STACK+4,y
        beq     L3D94
FOUT4:
        sta     $FF,y
L3D8F:
        lda     #$00
        sta     STACK,y
L3D94:
        lda     #$00
        ldy     #$01
        rts
; ----------------------------------------------------------------------------
CON_HALF:
        db   $80,$00,$00,$00
; ----------------------------------------------------------------------------
; POWERS OF 10 FROM 1E8 DOWN TO 1,
; AS 32-BIT INTEGERS, WITH ALTERNATING SIGNS
; ----------------------------------------------------------------------------
DECTBL:
        db   $FE,$79,$60 ; -100000
		  db	$00,$27,$10 ; 10000
		  db	$FF,$FC,$18 ; -1000
		  db	$00,$00,$64 ; 100
		  db	$FF,$FF,$F6 ; -10
		  db	$00,$00,$01 ; 1
DECTBL_END:
; ----------------------------------------------------------------------------
; "SQR" FUNCTION
; ----------------------------------------------------------------------------
SQR:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #lo(CON_HALF)
        ldy     #hi(CON_HALF)
        jsr     LOAD_FAC_FROM_YA
; ----------------------------------------------------------------------------
; EXPONENTIATION OPERATION
;
; ARG ^ FAC  =  EXP( LOG(ARG) * FAC )
; ----------------------------------------------------------------------------
FPWRT:
        beq     EXP
        lda     ARG
        bne     L3DD5
        jmp     STA_IN_FAC_SIGN_AND_EXP
L3DD5:
        ldx     #TEMP3
        ldy     #$00
        jsr     STORE_FAC_AT_YX_ROUNDED
        lda     ARGSIGN
        bpl     L3DEF
        jsr     INT
        lda     #TEMP3
        ldy     #$00
        jsr     FCOMP
        bne     L3DEF
        tya
        ldy     CHARAC
L3DEF:
        jsr     MFA
        tya
        pha
        jsr     LOG
        lda     #TEMP3
        ldy     #$00
        jsr     FMULT
        jsr     EXP
        pla
        lsr     a
        bcc     L3E0F
; ----------------------------------------------------------------------------
; NEGATE VALUE IN FAC
; ----------------------------------------------------------------------------
NEGOP:
        lda     FAC
        beq     L3E0F
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
L3E0F:
        rts
; ----------------------------------------------------------------------------
CON_LOG_E:
        db   $81,$38,$AA,$3B
POLY_EXP:
		  db	$06
		  db	$74,$63,$90,$8C
		  db	$77,$23,$0C,$AB
		  db	$7A,$1E,$94,$00
		  db	$7C,$63,$42,$80
		  db	$7E,$75,$FE,$D0
		  db	$80,$31,$72,$15
		  db	$81,$00,$00,$00
; ----------------------------------------------------------------------------
; "EXP" FUNCTION
;
; FAC = E ^ FAC
; ----------------------------------------------------------------------------
EXP:
        lda     #lo(CON_LOG_E)
        ldy     #hi(CON_LOG_E)
        jsr     FMULT
        lda     FACEXTENSION
        adc     #$50
        bcc     L3E4E
        jsr     INCREMENT_MANTISSA
L3E4E:
        sta     ARGEXTENSION
        jsr     MAF
        lda     FAC
        cmp     #$88
        bcc     L3E5C
L3E59:
        jsr     OUTOFRNG
L3E5C:
        jsr     INT
        lda     CHARAC
        clc
        adc     #$81
        beq     L3E59
        sec
        sbc     #$01
        pha
        ldx     #BYTES_FP
L3E6C:
        lda     ARG,x
        ldy     FAC,x
        sta     FAC,x
        sty     ARG,x
        dex
        bpl     L3E6C
        lda     ARGEXTENSION
        sta     FACEXTENSION
        jsr     FSUBT
        jsr     NEGOP
        lda     #lo(POLY_EXP)
        ldy     #hi(POLY_EXP)
        jsr     POLYNOMIAL
        lda     #$00
        sta     SGNCPR
        pla
        jsr     ADD_EXPONENTS1
        rts
; ----------------------------------------------------------------------------
; ODD POLYNOMIAL SUBROUTINE
;
; F(X) = X * P(X^2)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
;
; P(X^2) COMPUTED USING NORMAL POLYNOMIAL SUBROUTINE
; ----------------------------------------------------------------------------
POLYNOMIAL_ODD:
        sta     STRNG2
        sty     STRNG2+1
        jsr     STORE_FAC_IN_TEMP1_ROUNDED
        lda     #TEMP1X
        jsr     FMULT
        jsr     SERMAIN
        lda     #TEMP1X
        ldy     #$00
        jmp     FMULT
; ----------------------------------------------------------------------------
; NORMAL POLYNOMIAL SUBROUTINE
;
; P(X) = C(0)*X^N + C(1)*X^(N-1) + ... + C(N)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
; ----------------------------------------------------------------------------
POLYNOMIAL:
        sta     STRNG2
        sty     STRNG2+1
SERMAIN:
        jsr     STORE_FAC_IN_TEMP2_ROUNDED
        lda     (STRNG2),y
        sta     SERLEN
        ldy     STRNG2
        iny
        tya
        bne     L3EBA
        inc     STRNG2+1
L3EBA:
        sta     STRNG2
        ldy     STRNG2+1
L3EBE:
        jsr     FMULT
        lda     STRNG2
        ldy     STRNG2+1
        clc
        adc     #BYTES_FP
        bcc     L3ECB
        iny
L3ECB:
        sta     STRNG2
        sty     STRNG2+1
        jsr     FADD
        lda     #TEMP2
        ldy     #$00
        dec     SERLEN
        bne     L3EBE
RTS19:
        rts
; ----------------------------------------------------------------------------
; "RND" FUNCTION
; ----------------------------------------------------------------------------
CONRND1:
        db   $98,$35,$44,$7A
CONRND2:
        db   $68,$28,$B1,$46
RND:
        jsr     SIGN
        tax
        bmi     L3F01
        lda     #lo(RNDSEED)
        ldy     #hi(RNDSEED)
        jsr     LOAD_FAC_FROM_YA
        txa
        beq     RTS19
        lda     #lo(CONRND1)
        ldy     #hi(CONRND1)
        jsr     FMULT
        lda     #lo(CONRND2)
        ldy     #hi(CONRND2)
        jsr     FADD
L3F01:
        ldx     FAC_LAST
        lda     FAC+1
        sta     FAC_LAST
        stx     FAC+1
        lda     #$00
        sta     FACSIGN
        lda     FAC
        sta     FACEXTENSION
        lda     #$80
        sta     FAC
        jsr     NORMALIZE_FAC2
        ldx     #lo(RNDSEED)
        ldy     #hi(RNDSEED)
GOMOVMF:
        jmp     STORE_FAC_AT_YX_ROUNDED
; ----------------------------------------------------------------------------
; "COS" FUNCTION
; ----------------------------------------------------------------------------
COS:
        lda     #lo(CON_PI_HALF)
        ldy     #hi(CON_PI_HALF)
        jsr     FADD
; ----------------------------------------------------------------------------
; "SIN" FUNCTION
; ----------------------------------------------------------------------------
SIN:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #lo(CON_PI_DOUB)
        ldy     #hi(CON_PI_DOUB)
        ldx     ARGSIGN
        jsr     DIV
        jsr     COPY_FAC_TO_ARG_ROUNDED
        jsr     INT
        lda     #$00
        sta     STRNG1
        jsr     FSUBT
; ----------------------------------------------------------------------------
; (FAC) = ANGLE AS A FRACTION OF A FULL CIRCLE
;
; NOW FOLD THE RANGE INTO A QUARTER CIRCLE
;
; <<< THERE ARE MUCH SIMPLER WAYS TO DO THIS >>>
; ----------------------------------------------------------------------------
        lda     #lo(QUARTER)
        ldy     #hi(QUARTER)
        jsr     FSUB
        lda     FACSIGN
        pha
        bpl     SIN1
        jsr     FADDH
        lda     FACSIGN
        bmi     L3F5B
        lda     CPRMASK
        eor     #$FF
        sta     CPRMASK
; ----------------------------------------------------------------------------
; IF FALL THRU, RANGE IS 0...1/2
; IF BRANCH HERE, RANGE IS 0...1/4
; ----------------------------------------------------------------------------
SIN1:
        jsr     NEGOP
; ----------------------------------------------------------------------------
; IF FALL THRU, RANGE IS -1/2...0
; IF BRANCH HERE, RANGE IS -1/4...0
; ----------------------------------------------------------------------------
L3F5B:
        lda     #lo(QUARTER)
        ldy     #hi(QUARTER)
        jsr     FADD
        pla
        bpl     L3F68
        jsr     NEGOP
L3F68:
        lda     #lo(POLY_SIN)
        ldy     #hi(POLY_SIN)
        jmp     POLYNOMIAL_ODD
; ----------------------------------------------------------------------------
; "TAN" FUNCTION
;
; COMPUTE TAN(X) = SIN(X) / COS(X)
; ----------------------------------------------------------------------------
TAN:
        jsr     STORE_FAC_IN_TEMP1_ROUNDED
        lda     #$00
        sta     CPRMASK
        jsr     SIN
        ldx     #TEMP3
        ldy     #$00
        jsr     GOMOVMF
        lda     #TEMP1+(5-BYTES_FP)
        ldy     #$00
        jsr     LOAD_FAC_FROM_YA
        lda     #$00
        sta     FACSIGN
        lda     CPRMASK
        jsr     TAN1
        lda     #TEMP3
        ldy     #$00
        jmp     FDIV
TAN1:
        pha
        jmp     SIN1
; ----------------------------------------------------------------------------
CON_PI_HALF:
        db   $81,$49,$0F,$DB
CON_PI_DOUB:
        db   $83,$49,$0F,$DB
QUARTER:
        db   $7F,$00,$00,$00
POLY_SIN:
        db   $04,$86,$1E,$D7,$FB,$87,$99,$26
        db   $65,$87,$23,$34,$58,$86,$A5,$5D
        db   $E1,$83,$49,$0F,$DB

; ----------------------------------------------------------------------------
; "ATN" FUNCTION
; ----------------------------------------------------------------------------
ATN:
        lda     FACSIGN
        pha
        bpl     L3FDB
        jsr     NEGOP
L3FDB:
        lda     FAC
        pha
        cmp     #$81
        bcc     L3FE9
        lda     #lo(CON_ONE)
        ldy     #hi(CON_ONE)
        jsr     FDIV
; ----------------------------------------------------------------------------
; 0 <= X <= 1
; 0 <= ATN(X) <= PI/8
; ----------------------------------------------------------------------------
L3FE9:
        lda     #lo(POLY_ATN)
        ldy     #hi(POLY_ATN)
        jsr     POLYNOMIAL_ODD
        pla
        cmp     #$81
        bcc     L3FFC
        lda     #lo(CON_PI_HALF)
        ldy     #hi(CON_PI_HALF)
        jsr     FSUB
L3FFC:
        pla
        bpl     L4002
        jmp     NEGOP
L4002:
        rts
; ----------------------------------------------------------------------------
POLY_ATN:
        db   $08
		  db	$78,$3A,$C5,$37
		  db	$7B,$83,$A2,$5C
		  db	$7C,$2E,$DD,$4D
		  db	$7D,$99,$B0,$1E
		  db	$7D,$59,$ED,$24
		  db	$7E,$91,$72,$00
		  db	$7E,$4C,$B9,$73
		  db	$7F,$AA,$AA,$53
		  db	$81,$00,$00,$00
GENERIC_CHRGET:
        inc     TXTPTR
        bne     GENERIC_CHRGOT
        inc     TXTPTR+1
GENERIC_CHRGOT:
GENERIC_TXTPTR = GENERIC_CHRGOT + 1
        lda     $EA60
        cmp     #$3A
        bcs     L4058
GENERIC_CHRGOT2:
        cmp     #$20
        beq     GENERIC_CHRGET
        sec
        sbc     #$30
        sec
        sbc     #$D0
L4058:
        rts
GENERIC_RNDSEED:
; random number seed
        db   $80,$4F,$C7,$52
GENERIC_CHRGET_END:
; ----------------------------------------------------------------------------
PR_WRITTEN_BY:
        lda     #lo(QT_WRITTEN_BY)
        ldy     #hi(QT_WRITTEN_BY)
        jsr     STROUT
COLD_START:
        ldx     #$FF
        stx     CURLIN+1
        txs
        lda     #lo(COLD_START)
        ldy     #hi(COLD_START)
        sta     GORESTART+1
        sty     GORESTART+2
        sta     GOSTROUT+1
        sty     GOSTROUT+2
        lda     #lo(AYINT)
        ldy     #hi(AYINT)
        sta     GOAYINT
        sty     GOAYINT+1
        lda     #lo(GIVAYF)
        ldy     #hi(GIVAYF)
        sta     GOGIVEAYF
        sty     GOGIVEAYF+1
        lda     #$4C
        sta     GORESTART
        sta     GOSTROUT
        sta     JMPADRS
        sta     USR
        lda     #lo(IQERR)
        ldy     #hi(IQERR)
        sta     USR+1
        sty     USR+2
        lda     #WIDTH
        sta     Z17
        lda     #WIDTH2
        sta     Z18
        ldx     #GENERIC_CHRGET_END-GENERIC_CHRGET
L4098:
        lda     GENERIC_CHRGET-1,x
        sta     CHRGET-1,x
        dex
        bne     L4098
        txa
        sta     SHIFTSIGNEXT
        sta     LASTPT+1
        sta     Z15
        sta     POSX
        pha
        sta     Z14
        lda     #$03
        sta     DSCLEN
        lda     #$2C
        sta     LINNUM+1
        jsr     CRDO
        ldx     #TEMPST
        stx     TEMPPT
        lda     #lo(QT_MEMORY_SIZE)
        ldy     #hi(QT_MEMORY_SIZE)
        jsr     STROUT
        jsr     NXIN
        stx     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGET
        cmp     #$41
        beq     PR_WRITTEN_BY
        tay
        bne     L40EE
        lda     #lo(RAMSTART2)
        ldy     #hi(RAMSTART2)
        sta     LINNUM
        sty     LINNUM+1
        ldy     #$00
L40D7:
        inc     LINNUM
        bne     L40DD
        inc     LINNUM+1
L40DD:
        lda     #$92 ; 10010010 / 00100100
        sta     (LINNUM),y
        cmp     (LINNUM),y
        bne     L40FA
        asl     a
        sta     (LINNUM),y
        cmp     (LINNUM),y
        beq     L40D7; old: faster
        bne     L40FA
L40EE:
        jsr     CHRGOT
        jsr     LINGET
        tay
        beq     L40FA
        jmp     SYNERR
L40FA:
        lda     LINNUM
        ldy     LINNUM+1
        sta     MEMSIZ
        sty     MEMSIZ+1
        sta     FRETOP
        sty     FRETOP+1
L4106:
        lda     #lo(QT_TERMINAL_WIDTH)
        ldy     #hi(QT_TERMINAL_WIDTH)
        jsr     STROUT
        jsr     NXIN
        stx     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGET
        tay
        beq     L4136
        jsr     LINGET
        lda     LINNUM+1
        bne     L4106
        lda     LINNUM
        cmp     #$10
        bcc     L4106
        sta     Z17
L4129:
        sbc     #$0E
        bcs     L4129
        eor     #$FF
        sbc     #$0C
        clc
        adc     Z17
        sta     Z18
L4136:
        ldx     #lo(RAMSTART2)
        ldy     #hi(RAMSTART2)
        stx     TXTTAB
        sty     TXTTAB+1
        ldy     #$00
        tya
        sta     (TXTTAB),y
        inc     TXTTAB
        bne     L4192
        inc     TXTTAB+1
L4192:
        lda     TXTTAB
        ldy     TXTTAB+1
        jsr     REASON
        jsr     CRDO
        lda     MEMSIZ
        sec
        sbc     TXTTAB
        tax
        lda     MEMSIZ+1
        sbc     TXTTAB+1
        jsr     LINPRT
        lda     #lo(QT_BYTES_FREE)
        ldy     #hi(QT_BYTES_FREE)
        jsr     STROUT
        lda     #lo(STROUT)
        ldy     #hi(STROUT)
        sta     GOSTROUT+1
        sty     GOSTROUT+2
        jsr     SCRTCH
        lda     #lo(RESTART)
        ldy     #hi(RESTART)
        sta     GORESTART+1
        sty     GORESTART+2
        jmp     (GORESTART+1)
; OSI is compiled for ROM, but includes
; this unused string
        db   "WANT SIN-COS-TAN-ATN"
        db   0
QT_WRITTEN_BY:
        db   CR,LF,$0C ; FORM FEED
        db   "WRITTEN BY RICHARD W. WEILAND."
        db   CR,LF,0
QT_MEMORY_SIZE:
        db   "MEMORY SIZE"
        db   0
QT_TERMINAL_WIDTH:
        db   "TERMINAL WIDTH"
        db   0
QT_BYTES_FREE:
        db   " BYTES FREE"
        db   CR,LF,CR,LF
        db   "OSI 6502 BASIC VERSION 1.0 REV 3.2"
        db   CR,LF
        db   "COPYRIGHT 1977 BY MICROSOFT CO."
        db   CR,LF,0
        
; ----------------------------------------------------------------------
; 6502 System Monitor                    
; based on SyMon III (c)1990,2007 By Brian M. Phelps 
; ----------------------------------------------------------------------

; Zero-page storage for Hex file download subroutine
DPL         =     $A0             ; data pointer (two bytes)
DPH         =     $A1             ; high of data pointer
RECLEN      =     $A2             ; record length in bytes
START_LO    =     $A3
START_HI    =     $A4
RECTYPE     =     $A5
CHKSUM      =     $A6             ; record checksum accumulator
DLFAIL      =     $A7             ; flag for download failure
MTEMP4      =     $A8             ; save hex value
        
; 14 byte buffer
INBUFF      =     $B0             ; 14 bytes ($B0-$BD) 
; 
; 16-bit variables:
COMLO       =     $BE
COMHI       =     COMLO+1
PROLO       =     $C2
PROHI       =     PROLO+1
BUFADR      =     $C4
BUFADRH     =     BUFADR+1
MTEMP2      =     $C6
MTEMP2H     =     MTEMP2+1
MINDEX      =     $C8
MINDEXH     =     MINDEX+1
MTEMP3      =     $CA
MTEMP3H     =     MTEMP3+1
; 
; 8-bit variables and constants:
MTEMP       =     $CC
BUFIDX      =     $CD
BUFLEN      =     $CE
BSPIND      =     $CF
IDX         =     $D0
IDY         =     $D1
SCNT        =     $D2
SMTEMP      =     $D5
RT1         =     $D6
RT2         =     $D7
LOKOUT      =     $D9
ACCUM       =     $DA
XREG        =     $DB
YREG        =     $DC
SREG        =     $DD
PREG        =     $DE
POINTER     =     $DF
HPHANTOM    =     $E0           ; HPHANTOM MUST be located (in target memory) immediatly below the HEX0AND1 variable
HEX0AND1    =     $E1
HEX2AND3    =     $E2
HEX4AND5    =     $E3
HEX6AND7    =     $E4
DPHANTOM    =     $E4           ; DPHANTOM MUST be located (in target memory) immediatly below the DEC0AND1 variable
DEC0AND1    =     $E5
DEC2AND3    =     $E6
DEC4AND5    =     $E7
DEC6AND7    =     $E8
DEC8AND9    =     $E9
INQTY       =     $EA
AINTSAV     =     $ED
XINTSAV     =     $EE
YINTSAV     =     $EF


            org $E000

; ----------------------------------------------------------------------
; Intel Hex file download
; ----------------------------------------------------------------------
HEXDNLD:    LDA #0
            STA DLFAIL          ; start by assuming no D/L failure
            JSR PUTSTR
            db CR,LF,CR,LF
            db "Send Intel Hex format 6502 object code: ",0
HDWRECS:    JSR GETSER          ; get a character from the serial port
            CMP #':'            ; start of record?
            BEQ STARTDL
            CMP #$1B            ; Escape?
            BEQ HEXEXIT
HDWRECS1:   CMP #$03            ; Control C?
            BNE HDWRECS         ; go back for another character  
HEXEXIT:    RTS
        
; start of record marker has been detected, get the record length
STARTDL:    JSR GETHEX          ; Get the record length
            STA RECLEN          ; save it
            STA CHKSUM          ; and save first byte of checksum
            JSR GETHEX          ; Get the high part of start address
            STA START_HI
            CLC
            ADC CHKSUM          ; Add in the checksum
            STA CHKSUM          ; 
            JSR GETHEX          ; Get the low part of the start address
            STA START_LO
            CLC
            ADC CHKSUM
            STA CHKSUM
            JSR GETHEX          ; Get the record type
            STA RECTYPE         ; & save it
            CLC
            ADC CHKSUM
            STA CHKSUM
            LDA RECTYPE
            BNE HDER1           ; end-of-record
            LDX RECLEN          ; number of data bytes to write to memory
            LDY #0              ; start offset at 0
HDLP1:      JSR GETHEX          ; Get the first/next/last data byte
            STA (START_LO),y    ; Save it to RAM
            CLC
            ADC CHKSUM
            STA CHKSUM          ; 
            INY                 ; update data pointer
            DEX                     ; decrement count
            BNE HDLP1
            JSR GETHEX          ; get the checksum
            CLC
            ADC CHKSUM
            BNE HDDLF1          ; If failed, report it
            
            ; Another successful record has been processed
            LDA #'.'            ; Character indicating record OK = '.'
            STA ACIAData        ; write it out but don't wait for output
            JMP HDWRECS         ; get next record
        
HDDLF1:     LDA #'F'            ; Character indicating record failure = 'F'
            STA DLFAIL          ; download failed if non-zero
            STA ACIAData        ; write it to transmit buffer register
            JMP HDWRECS         ; wait for next record start
            
HDER1:      CMP #1              ; Check for end-of-record type
            BEQ HDER2
            JSR PUTSTR         ; Warn user of unknown record type
            db CR,LF,"Unknown record type $",CR,LF,0
            LDA RECTYPE         ; Get it
            STA	DLFAIL		    ; non-zero --> download has failed
            JSR PUTHEX          ; print it
            LDA #13		        ; but we'll let it finish so as not to
            JSR PUTSER		    ; falsely start a new d/l from existing
            LDA #10		        ; file that may still be coming in for
            JSR PUTSER		    ; quite some time yet.
            JMP	HDWRECS
        
            ; We've reached the end-of-record record
HDER2:      JSR GETHEX          ; get the checksum
            CLC
            ADC CHKSUM          ; Add previous checksum accumulator value
            BEQ HDER3           ; checksum = 0 means we're OK!
            JSR PUTSTR          ; Warn user of bad checksum
            db  CR,LF,LF,"Bad record checksum!",CR,LF,"Aborting!",CR,LF,0
            RTS

HDER3:      LDA DLFAIL
            BEQ HDEROK
            ; A download failure has occurred
            JSR PUTSTR
            db CR,LF,LF,"Download Failed",CR,LF,"Aborting!",CR,LF,0
            RTS

HDEROK:     JSR PUTSTR
            db CR,LF,LF,"Download Successful!",CR,LF,0
            
            lda START_LO
            ora START_HI
            bne HDEROK1             ; branch if the address in the last record is not zero
            rts                     ; else, return to caller
            
HDEROK1:    JMP (START_LO)         ; jump to the address in last record

GETSER:     JMP CHIN

; get one hex digit
GETHEX:     JSR     GETSER
            JSR     MKNIBL  	    ; Convert to 0..F numeric
            ASL     a
            ASL     a
            ASL     a
            ASL     a       	    ; This is the upper nibble
            AND     #$F0
            STA     MTEMP4
            JSR     GETSER
            JSR     MKNIBL
            ORA     MTEMP4
            RTS             	    ; return with the nibble received

; Convert the ASCII nibble to numeric value from 0-F:
MKNIBL:     CMP     #'9'+1  	    ; See if it's 0-9 or 'A'..'F' (no lowercase yet)
            BCC     MKNNH   	    ; If we borrowed, we lost the carry so 0..9
            SBC     #7+1    	    ; Subtract off extra 7 (SBC subtracts off one less)
            ; If we fall through, carry is set unlike direct entry at MKNNH
MKNNH:      SBC     #'0'-1  	    ; subtract off '0' (if carry clear coming in)
            AND     #$0F    	    ; no upper nibble no matter what
            RTS             	    ; and return the nibble

; Put byte in A as hexydecascii
PUTHEX:     PHA
            LSR A
            LSR A
            LSR A
            LSR A
            JSR     PRNIBL
            PLA
PRNIBL:     and     #$0F    	    ; strip off the low nibble
            CMP     #$0A
            BCC     NOTHEX  	    ; if it's 0-9, add '0' else also add 7
            ADC     #6      	    ; Add 7 (6+carry=1), result will be carry clear
NOTHEX:     ADC     #'0'    	    ; If carry clear, we're 0-9
; Write the character in A as ASCII:
PUTSER:     JMP     COUT
        
; ----------------------------------------------------------------------        
; Print the null-terminated string following in-line
; ----------------------------------------------------------------------
PUTSTR:     PLA			            ; Get the low part of "return" address (data start address)
            STA     DPL
            PLA
            STA     DPH             ; Get the high part of "return" address
                                    ; (data start address) Note: actually we're pointing one short
PSINB:      LDY     #1
            LDA     (DPL),y         ; Get the next string character
            inc     DPL             ; update the pointer
            BNE     PSICHO          ; if not, we're pointing to next character
            inc     DPH             ; account for page crossing
PSICHO:     ORA     #0              ; Set flags according to contents of Accumulator
            BEQ     PSIX1           ; don't print the final NULL
            JSR     PUTSER          ; write it out
            JMP     PSINB           ; back around
PSIX1:      inc     DPL
            BNE     PSIX2
            inc     DPH             ; account for page crossing
PSIX2:      JMP     (DPL)           ; return to byte following final NULL

; ----------------------------------------------------------------------
; ASC2BN subroutine: Convert 2 ASCII HEX digits to a binary (byte) value.
; Enter:ACCUMULATOR = high digit, Y-REGISTER = low digit
; Returns:ACCUMULATOR = binary value 
; ----------------------------------------------------------------------
ASC2BN:  PHA                        ; Save high digit on STACK
         TYA                        ; Copy low digit to ACCUMULATOR
         JSR  BINARY                ; Convert digit to binary nybble:result in low nybble
         STA  MTEMP                  ; Store low nybble
         PLA                        ; Pull high digit from STACK
         JSR  BINARY                ; Convert digit to binary nybble:result in low nybble
         ASL  A                     ; Move low nybble to high nybble 
         ASL  A
         ASL  A
         ASL  A
         ORA  MTEMP                  ; OR high nybble with stored low nybble 
         RTS                        ; Done ASC2BN subroutine, RETURN
BINARY:  SEC                        ; Clear BORROW
         SBC  #$30                  ; Subtract $30 from ASCII HEX digit
         CMP  #$0A                  ; GOTO BNOK IF result < $0A     
         BCC  BNOK
         SBC  #$07                  ; ELSE, subtract $07 from result
BNOK:    RTS                        ; Done BINARY subroutine, RETURN

; ----------------------------------------------------------------------
; ASCTODEC subroutine: convert ASCII DECIMAL digits to BCD
; ----------------------------------------------------------------------
ASCTODEC:LDA  #$00                  ; Initialize (zero) BCD digit input buffer:
         STA  DEC0AND1              ; two most significant BCD digits
         STA  DEC2AND3
         STA  DEC4AND5
         STA  DEC6AND7
         STA  DEC8AND9              ; two least significant BCD digits
         LDX  BUFIDX                ; Read number of digits entered:ASCII digit buffer MINDEX
         BEQ  A2DDONE               ; GOTO A2DDONE IF BUFIDX = 0:no digits were entered
         LDY  #$05                  ; ELSE, Initialize BCD input buffer MINDEX:process up to 5 BCD bytes (10 digits)
ATODLOOP:JSR  A2DSUB                ; Read ASCII digit then convert to BCD 
         STA  DPHANTOM,Y            ; Write BCD digit to MINDEXed buffer location (MINDEX always > 0)
         JSR  A2DSUB                ; Read ASCII digit then convert to BCD 
         ASL  A                     ; Make this BCD digit the more significant in the BCD byte 
         ASL  A
         ASL  A
         ASL  A         
         ORA  DPHANTOM,Y            ; OR with the less significant digit  
         STA  DPHANTOM,Y            ; Write BCD byte to MINDEXed buffer location (MINDEX always > 0)
         DEY                        ; Decrement BCD input buffer MINDEX 
         BNE  ATODLOOP              ; GOTO ATODLOOP IF buffer MINDEX <> 0:there is room to process another digit
A2DDONE: RTS                        ; ELSE, done ASCTODEC, RETURN
; Read MINDEXed ASCII DECIMAL digit from text buffer then convert digit to 4 bit BCD
A2DSUB:  TXA                        ; GOTO A2DCONV IF digit buffer MINDEX <> 0:there are more digits to process
         BNE  A2DCONV
         PLA                        ; ELSE, pull return address from STACK
         PLA
         RTS                        ; Done ASCTODEC, RETURN
A2DCONV: LDA  INBUFF-1,X            ; Read MINDEXed ASCII DECIMAL digit 
         SEC                        ; Subtract ASCII "0" from ASCII DECIMAL digit:convert digit to BCD
         SBC  #$30
         DEX                        ; Decrement ASCII digit buffer MINDEX
         RTS                        ; A2DSUB done, RETURN

; ----------------------------------------------------------------------
; BCDOUT subroutine: convert 10 BCD digits to ASCII DECIMAL digits then send result to terminal. 
; Leading zeros are supressed in the displayed result.
; Call with 10 digit (5 byte) BCD value contained in variables DEC0AND1 through DEC8AND9:
; DEC0AND1 ($E5) Two most significant BCD digits
; DEC2AND3 ($E6)
; DEC4AND5 ($E7)
; DEC6AND7 ($E8)
; DEC8AND9 ($E9) Two least significant BCD digits
; ----------------------------------------------------------------------
BCDOUT:  LDX  #$00                  ; Initialize BCD output buffer MINDEX:point to MSB
         LDY  #$00                  ; Initialize leading zero flag:no non-zero digits have been processed
BCDOUTL: LDA  DEC0AND1,X            ; Read MINDEXed byte from BCD output buffer
         LSR  A                     ; Shift high digit to low digit, zero high digit
         LSR  A
         LSR  A
         LSR  A
         JSR  BCDTOASC              ; Convert BCD digit to ASCII DECIMAL digit, send digit to terminal
         LDA  DEC0AND1,X            ; Read MINDEXed byte from BCD output buffer
         AND  #$0F                  ; Zero the high digit
         JSR  BCDTOASC              ; Convert BCD digit to ASCII DECIMAL digit, send digit to terminal
         INX                        ; Increment BCD output buffer MINDEX
         CPX  #$05
         BNE  BCDOUTL               ; LOOP back to BCDOUTL IF output buffer MINDEX <> 5
         CPY  #$00
         BNE  BCDOUTDN              ; ELSE, GOTO BCDOUTDN IF any non-zero digits were processed
         LDA  #$30                  ; ELSE, send "0" to terminal
         JSR  COUT
BCDOUTDN:RTS                        ; Done BCDOUT subroutine, RETURN

; ----------------------------------------------------------------------
; BCDTOASC subroutine: 
; convert BCD digit to ASCII DECIMAL digit, send digit to terminal IF it's not a leading zero
; ----------------------------------------------------------------------
BCDTOASC:BNE  NONZERO               ; GOTO NONZERO IF BCD digit <> 0
         CPY  #$00                  ; ELSE, GOTO BTADONE IF no non-zero digits have been processed 
         BEQ  BTADONE               ; (supress output of leading zeros)
NONZERO: INY                        ; ELSE, indicate that a non-zero digit has been processed (Y REGISTER <> 0) 
         CLC                        ; Add ASCII "0" to digit:convert BCD digit to ASCII DECIMAL digit
         ADC  #$30
         JSR  COUT                  ; Send converted digit to terminal
BTADONE: RTS                        ; Done BCDTOASC subroutine, RETURN

; ----------------------------------------------------------------------
; BCDTOHEX subroutine: convert a 1-10 digit BCD value to a 1-8 digit HEX value.
; Call with 10 digit (5 byte) DECIMAL value in DEC0AND1(MSB) through DEC8AND9(LSB).
; Returns with 8 digit (4 byte) HEX result in HEX0AND1(MSB) through HEX6AND7(LSB)
; DPHANTOM is a 16 bit address used to reference an 8 bit zero-page address.
; (BCDTOHEX needs LDA $hh,Y (an invalid instruction) so we use LDA $00hh,Y instead)
; This address is not written-to nor read-from in the BCDTOHEX subroutine.
; The address is the zero-page memory location immediatly below the DEC0AND1 variable
; BCD value input buffer:
; DEC0AND1 ; Two most significant BCD digits
; DEC2AND3
; DEC4AND5
; DEC6AND7
; DEC8AND9 ; Two least significant BCD digits
; HEX value output buffer (HEX accumulator):
; HEX0AND1 Two most significant HEX digits
; HEX2AND3
; HEX4AND5
; HEX6AND7 Two least significant HEX digits
; ----------------------------------------------------------------------
BCDTOHEX:LDA  #$00                  ; Initialize (zero) output buffer. This is an 8 digit (4 byte) HEX accumulator
         STA  HEX0AND1
         STA  HEX2AND3
         STA  HEX4AND5
         STA  HEX6AND7
         LDY  #$05                  ; Initialize DECIMAL input buffer byte MINDEX:point to (address - 1) of LSB
         LDX  #$03                  ; Initialize multiplicand table MINDEX:point to LSB of lowest multiplicand
BCDLOOP: LDA  DPHANTOM,Y            ; Read MINDEXed byte from input buffer:Y REGISTER MINDEX always > 0 here
         AND  #$0F                  ; Zero the high digit
         JSR  MULTPLI               ; Multiply low digit
         INX                        ; Add 4 to multiplicand table MINDEX:point to LSB of next higher multiplicand
         INX
         INX
         INX
         LDA  DPHANTOM,Y            ; Read MINDEXed byte from input buffer:Y REGISTER MINDEX always > 0 here 
         LSR  A                     ; Shift high digit to low digit, zero high digit
         LSR  A
         LSR  A
         LSR  A
         JSR  MULTPLI               ; Multiply digit
         INX                        ; Add 4 to multiplicand table MINDEX:point to LSB of next higher multiplicand
         INX
         INX
         INX
         DEY                        ; Decrement DECIMAL input buffer byte MINDEX
         BNE  BCDLOOP               ; LOOP back to BCDLOOP IF byte MINDEX <> 0:there are more bytes to process
         RTS                        ; ELSE, done BCDTOHEX subroutine, RETURN
         
; ----------------------------------------------------------------------
; Multiply MINDEXed multiplicand by digit in ACCUMULATOR
; ----------------------------------------------------------------------
MULTPLI: JSR  SAVREGS               ; Save A,X,Y REGISTERS on STACK
         TAY                        ; Copy digit to Y REGISTER:multiplier loop counter
DMLTLOOP:CPY  #$00
         BNE  DDOADD                ; GOTO DDOADD IF multiplier loop counter <> 0 
         JSR  RESREGS               ; ELSE, pull A,X,Y REGISTERS from STACK
         RTS                        ; Done MULTIPLI subroutine, RETURN
; Add MINDEXed multiplicand to HEX accumulator (output buffer)
DDOADD:  CLC
         LDA  DMULTAB,X             ; Least significant byte of MINDEXed multiplicand
         ADC  HEX6AND7              ; Least significant byte of HEX accumulator
         STA  HEX6AND7
         LDA  DMULTAB-1,X
         ADC  HEX4AND5
         STA  HEX4AND5
         LDA  DMULTAB-2,X
         ADC  HEX2AND3
         STA  HEX2AND3
         LDA  DMULTAB-3,X           ; Most significant byte of MINDEXed multiplicand
         ADC  HEX0AND1              ; Most significant byte of HEX accumulator
         STA  HEX0AND1
         DEY                        ; Decrement multiplier loop counter
         BCS  OVRFLOW               ; GOTO OVRFLOW IF the last add produced a CARRY:HEX output buffer has OVRFLOWed 
         BCC  DMLTLOOP              ; ELSE, LOOP back to DMLTLOOP (always branch)
OVRFLOW: LDA  #$2A                  ; Send "*" to terminal:indicate that an OVRFLOW has occured
         JSR  COUT
         JMP  DMLTLOOP ; LOOP back to DMLTLOOP  
; HEX multiplicand table:
DMULTAB: db  $00, $00, $00, $01    ; HEX weight of least significant BCD digit
         db  $00, $00, $00, $0A
         db  $00, $00, $00, $64
         db  $00, $00, $03, $E8
         db  $00, $00, $27, $10
         db  $00, $01, $86, $A0
         db  $00, $0F, $42, $40
         db  $00, $98, $96, $80
         db  $05, $F5, $E1, $00
         db  $3B, $9A, $CA, $00    ; HEX weight of most significant BCD digit

; ----------------------------------------------------------------------
; BEEP subroutine: Send ASCII [BELL] to terminal
; ----------------------------------------------------------------------
BEEP:    PHA                        ; Save ACCUMULATOR on STACK
         LDA  #$07                  ; Send ASCII [BELL] to terminal
         JSR  COUT
         PLA                        ; Restore ACCUMULATOR from STACK
         RTS                        ; Done BEEP subroutine, RETURN

; ----------------------------------------------------------------------
; BN2ASC subroutine: Convert byte in ACCUMULATOR to two ASCII HEX digits.
; Returns:ACCUMULATOR = high digit, Y-REGISTER = low digit
; ----------------------------------------------------------------------
BN2ASC:  TAX                        ; Copy ACCUMULATOR to X-REGISTER
         AND  #$F0                  ; Mask (zero) low nybble
         LSR  A                     ; Move high nybble to low nybble:high nybble now = 0
         LSR  A
         LSR  A
         LSR  A
         JSR  ASCII                 ; Convert nybble to an ASCII HEX digit
         PHA                        ; Save high digit on STACK
         TXA                        ; Restore ACCUMULATOR from X-REGISTER
         AND  #$0F                  ; Mask (zero) high nybble
         JSR  ASCII                 ; Convert nybble to an ASCII HEX digit
         TAY                        ; Copy low digit to Y-REGISTER
         PLA                        ; Pull high digit from STACK to ACCUMULATOR
         RTS                        ; Done BN2ASC subroutine, RETURN
ASCII:   CMP  #$0A                  ; GOTO ASOK IF nybble < $A
         BCC  ASOK
         CLC                        ; ELSE, clear CARRY
         ADC  #$07                  ; ADD $07
ASOK:    ADC  #$30                  ; ADD $30
         RTS                        ; Done BN2ASC or ASCII subroutine, RETURN

; ----------------------------------------------------------------------
; BSOUT subroutine: send [BACKSPACE] to terminal
; ----------------------------------------------------------------------
BSOUT:   LDA  #$08                  ; Send [BACKSPACE] to terminal
         JMP  COUT                  ; then done BSOUT subroutine, RETURN

; ----------------------------------------------------------------------
; CHIN subroutine: Wait for a keystroke, return with keystroke in ACCUMULATOR
; ----------------------------------------------------------------------
CHIN:    LDA	ACIAStatus
         AND	#1
         CMP	#1
         BNE	CHIN
         LDA	ACIAData
         RTS

; ----------------------------------------------------------------------
; CHREG subroutine: Display HEX byte value in ACCUMULATOR, r=est HEX byte input from terminal
; ----------------------------------------------------------------------
CHREG:   JSR  PRBYTE                ; Display HEX value of ACCUMULATOR
         JSR  SPC                   ; Send [SPACE] to terminal
         JMP  HEXIN2                ; R=est HEX byte input from terminal (result in ACCUMULATOR)
                                    ; then done CHREG subroutine, RETURN

; ----------------------------------------------------------------------
; COUT subroutines:Send byte in ACCUMULATOR to terminal (1,2 or 3 times)
; ----------------------------------------------------------------------
COUT3:   JSR  COUT                  ; (Send byte 3 times)
COUT2:   JSR  COUT                  ; (Send byte 2 times)
COUT:    PHA                        ; Save ACCUMULATOR on STACK
COUTL:   LDA	ACIAStatus
         AND	#2
         CMP	#2
         BNE	COUTL
         PLA
         STA	ACIAData
         RTS

; ----------------------------------------------------------------------
; DECINPUT subroutine: r=est 1 to 10 DECIMAL digits from terminal. Result is
; stored in zero-page address INBUFF through (INBUFF + $0A)  
; Setup RDLINE subroutine parameters:
; ----------------------------------------------------------------------
DECINPUT:LDA  #$FF                  ; Allow only valid ASCII DECIMAL digits to be entered:
         STA  LOKOUT                ; variable LOKOUT = $FF
         LDY  #INBUFF               ; Y-REGISTER = buffer address low byte
         LDA  #$00                  ; ACCUMULATOR = buffer address high byte
         LDX  #$0A                  ; X-REGISTER = maximum number of digits allowed
         JSR  RDLINE                ; R=est ASCII DECIMAL digit(s) input from terminal
         RTS                        ; Done DECINPUT subroutine, RETURN

; ----------------------------------------------------------------------
; DECMINDEX subroutine: decrement 16 bit system variable MINDEX,MINDEXH
; ----------------------------------------------------------------------
DECMINDEX:SEC
         LDA  MINDEX
         SBC  #$01
         STA  MINDEX
         LDA  MINDEXH
         SBC  #$00
         STA  MINDEXH
         RTS                        ; Done DECMINDEX subroutine, RETURN

; ----------------------------------------------------------------------
; DOLLAR subroutine: Send "$" to terminal
; ----------------------------------------------------------------------
DOLLAR:  PHA                        ; Save ACCUMULATOR on STACK
         LDA  #$24                  ; Send "$" to terminal
         BNE  SENDIT                ; GOTO SENDIT

; ----------------------------------------------------------------------
; CROUT subroutines:Send CR,LF to terminal (1 or 2 times)
; ----------------------------------------------------------------------
; Send CR,LF to terminal 2 times
CR2:     JSR  CROUT                 ; Send CR,LF to terminal
; Send CR,LF to terminal 1 time     
CROUT:   PHA                        ; Save ACCUMULATOR
         LDA  #$0D                  ; Send [RETURN] to terminal
         JSR  COUT
         LDA  #$0A                  ; Send [LINEFEED] to terminal
SENDIT:  JSR  COUT
         PLA                        ; Restore ACCUMULATOR from STACK
         RTS                        ; Done CROUT or DOLLAR subroutine, RETURN

; ----------------------------------------------------------------------
; GLINE subroutine: Send a horizontal line to terminal 
; ----------------------------------------------------------------------
GLINE:   LDX  #$4F
         LDA  #$7E                  ; Send "~" to terminal 79 times
GLINEL:  JSR  COUT
         DEX
         BNE  GLINEL
         RTS                        ; Done GLINE subroutine, RETURN

; ----------------------------------------------------------------------
; HEXIN subroutines:R=est 1 to 4 ASCII HEX digit input from terminal
; then convert digits into a binary (byte or word) value
; IF 1 OR 2 digits are R=ESTED, returns byte in ACCUMULATOR
; IF 3 OR 4 digits are R=ESTED, returns word in variable MINDEX (low byte),MINDEXH (high byte)
; Variable SCNT will contain the number of digits entered
; ----------------------------------------------------------------------
HEXIN2:  LDA  #$02                  ; R=est 2 ASCII HEX digits from terminal:8 bit value 
         BNE  HEXIN                 ; GOTO HEXIN:always branch
HEXIN4:  LDA  #$04                  ; R=est 4 ASCII HEX digits from terminal:16 bit value
HEXIN:   STA  INQTY                 ; Store number of digits to allow:value = 1 to 4 only
         JSR  DOLLAR                ; Send "$" to terminal
; Setup RDLINE subroutine parameters:
         LDA  #$80                  ; Allow only valid ASCII HEX digits to be entered:
         STA  LOKOUT                ; make variable LOKOUT <> 0
         LDA  #$00                  ; ACCUMULATOR = buffer address high byte 
         LDY  #INBUFF               ; Y-REGISTER = buffer address low byte
         LDX  INQTY                 ; X-REGISTER = maximum number of digits allowed
         JSR  RDLINE                ; R=est ASCII HEX digit(s) input from terminal
         STX  SCNT                  ; Save number of digits entered to SCNT
         LDA  INQTY                 ; GOTO JUST4 IF 4 digits were R=ESTED
         CMP  #$04
         BEQ  JUST4
         CMP  #$03                  ; ELSE, GOTO JUST3 IF 3 digits were R=ESTED
         BEQ  JUST3
         CPX  #$00                  ; ELSE, GOTO IN2 IF 0 OR 2 digits were entered
         BEQ  IN2
         CPX  #$02
         BEQ  IN2
         JSR  JUSTBYTE              ; ELSE, move digit from INBUFF to INBUFF+1, write "0" to INBUFF
IN2:     LDA  INBUFF                ; Convert 2 ASCII HEX digits in INBUFF(high digit),
         LDY  INBUFF+1              ; INBUFF+1(low digit) to a binary value,
         JMP  ASC2BN                ; (result in ACCUMULATOR) then RETURN (done HEXIN subroutine) 
JUST3:   CPX  #$00                  ; GOTO IN3 IF 0 OR 3 digits were entered
         BEQ  IN3
         CPX  #$03
         BEQ  IN3
         CPX  #$02                  ; ELSE, GOTO SHFT3 IF 2 digits were entered
         BEQ  SHFT3
         JSR  JUSTBYTE              ; ELSE, move digit from INBUFF to INBUFF+1, write "0" to INBUFF
SHFT3:   JSR  SHIFT                 ; Move digits from INBUFF+1 to INBUFF+2, INBUFF to INBUFF+1
IN3:     LDA  #$30                  ; Convert 2 ASCII HEX digits, "0"(high digit),
         LDY  INBUFF                ; INBUFF(low digit) to a binary value,
         JSR  ASC2BN                ; result in ACCUMULATOR
         STA  MINDEXH                ; Store in variable MINDEXH(high byte)
         LDA  INBUFF+1              ; Convert 2 ASCII HEX digits in INBUFF+1(high digit),
         LDY  INBUFF+2              ; INBUFF+2(low digit) to a binary value,
         JSR  ASC2BN                ; result in ACCUMULATOR
         STA  MINDEX                 ; Store in variable MINDEX(low byte)
         RTS                        ; Done HEXIN subroutine, RETURN
JUST4:   CPX  #$00                  ; GOTO IN4 IF 0 OR 4 digits were entered
         BEQ  IN4
         CPX  #$04
         BEQ  IN4
         CPX  #$03                  ; ELSE, GOTO X3 IF 3 digits were entered
         BEQ  X3
         CPX  #$02                  ; ELSE, GOTO X2 IF 2 digits were entered
         BEQ  X2
         JSR  JUSTBYTE              ; ELSE, move digit from INBUFF to INBUFF+1, write "0" to INBUFF
X2:      JSR  SHIFT                 ; Move digits from INBUFF+1 to INBUFF+2, INBUFF to INBUFF+1
X3:      LDA  INBUFF+2              ; Move digits from INBUFF+2 to INBUFF+3
         STA  INBUFF+3
         JSR  SHIFT                 ; Move digits from INBUFF+1 to INBUFF+2, INBUFF to INBUFF+1
IN4:     LDA  INBUFF                ; Convert 2 ASCII HEX digits in INBUFF(high digit),
         LDY  INBUFF+1              ; INBUFF+1(low digit) to a binary value,
         JSR  ASC2BN                ; result in ACCUMULATOR
         STA  MINDEXH                ; Store in variable MINDEXH(high byte)
         LDA  INBUFF+2              ; Convert 2 ASCII HEX digits in INBUFF+2(high digit),
         LDY  INBUFF+3              ; INBUFF+3(low digit) to a binary value,
         JSR  ASC2BN                ; result in ACCUMULATOR
         STA  MINDEX                 ; Store in variable MINDEX(low byte)
         RTS                        ; Done HEXIN subroutine, RETURN
SHIFT:   LDA  INBUFF+1              ; Move digit from INBUFF+1 to INBUFF+2
         STA  INBUFF+2
JUSTBYTE:LDA  INBUFF                ; Move digit from INBUFF to INBUFF+1
         STA  INBUFF+1
         LDA  #$30                  ; Write "0" to INBUFF
         STA  INBUFF
         RTS                        ; Done SHIFT or JUSTBYTE subroutine, RETURN

; ----------------------------------------------------------------------
; HEXINPUT subroutine: r=est 1 to 8 digit HEX value input from terminal then convert ASCII HEX to HEX
; Setup RDLINE subroutine parameters:
; ----------------------------------------------------------------------
HEXINPUT:LDA  #$80                  ; Allow only valid ASCII HEX digits to be entered:
         STA  LOKOUT                ; variable LOKOUT <> 0
         LDX  #$08                  ; X-REGISTER = maximum number of digits allowed
         LDA  #$00                  ; ACCUMULATOR = buffer address high byte
         LDY  #INBUFF               ; Y-REGISTER = buffer address low byte
         JSR  RDLINE                ; R=est ASCII HEX digit(s) input from terminal
; Convert ASCII HEX digits to HEX
ASCTOHEX:LDA  #$00                  ; Initialize (zero) HEX digit input buffer:
         STA  HEX0AND1              ; Two most significant HEX digits
         STA  HEX2AND3
         STA  HEX4AND5
         STA  HEX6AND7              ; Two least significant HEX digits
         LDY  #$04                  ; Initialize HEX input buffer MINDEX:process 4 bytes
ASCLOOP: STY  SCNT                  ; Save HEX input buffer MINDEX
         LDA  INBUFF-1,X            ; Read MINDEXed ASCII HEX digit from RDLINE buffer 
         TAY                        ; Copy digit to Y REGISTER:least significant digit
         LDA  #$30                  ; Make ACCUMULATOR = ASCII "0":MS digit 
         JSR  ASC2BN                ; Convert ASCII digits to binary value
         LDY  SCNT                  ; Read saved HEX input buffer MINDEX
         STA  HPHANTOM,Y            ; Write byte to MINDEXed HEX input buffer location 
         DEX                        ; Decrement ASCII digit count
         BEQ  HINDONE               ; GOTO HINDONE IF no ASCII digits are left to process
         LDY  #$30                  ; ELSE, make Y REGISTER = ASCII "0":LS digit
         LDA  INBUFF-1,X            ; Read MINDEXed ASCII HEX digit from RDLINE buffer:MS digit
         JSR  ASC2BN                ; Convert ASCII digits to binary value
         LDY  SCNT                  ; Read saved HEX input buffer MINDEX
         ORA  HPHANTOM,Y            ; OR high digit with low digit
         STA  HPHANTOM,Y            ; Write byte to MINDEXed HEX input buffer location 
         DEX                        ; Decrement ASCII digit count
         BEQ  HINDONE               ; GOTO HINDONE IF no ASCII digits left to process
         DEY                        ; ELSE, decrement HEX input buffer MINDEX
         BNE  ASCLOOP               ; LOOP back to ASCLOOP IF HEX input buffer is full  
HINDONE: RTS                        ; ELSE, Done HEXINPUT subroutine, RETURN

; ----------------------------------------------------------------------
; HEXOUT subroutine: convert 8 HEX digits to ASCII HEX digits then send result to terminal. 
; Leading zeros are supressed in the displayed result.
; Call with 8 digit (4 byte) HEX value contained in variables HEX0AND1 through HEX6AND7:
; HEX0AND1 Two most significant HEX digits
; HEX2AND3
; HEX4AND5
; HEX6AND7 Two least significant HEX digits
; ----------------------------------------------------------------------
HEXOUT:  LDX  #$00                  ; Initialize HEX output buffer MINDEX:point to MSB
         LDY  #$00                  ; Initialize leading zero flag:no non-zero digits have been processed
HEXOUTL: LDA  HEX0AND1,X            ; Read MINDEXed byte from HEX output buffer
         LSR  A                     ; Shift high digit to low digit, zero high digit
         LSR  A
         LSR  A
         LSR  A
         JSR  HEXTOASC              ; Convert HEX digit to ASCII HEX digit, send digit to terminal
         LDA  HEX0AND1,X            ; Read MINDEXed byte from HEX output buffer
         AND  #$0F                  ; Zero the high digit
         JSR  HEXTOASC              ; Convert HEX digit to ASCII HEX digit, send digit to terminal
         INX                        ; Increment HEX output buffer MINDEX
         CPX  #$04
         BNE  HEXOUTL               ; LOOP back to HEXOUTL IF output buffer MINDEX <> 4 
         CPY  #$00
         BNE  HEXOUTDN              ; ELSE, GOTO HEXOUTDN IF any non-zero digits were processed
         LDA  #$30                  ; ELSE, send "0" to terminal
         JSR  COUT
HEXOUTDN:RTS                        ; Done HEXOUT subroutine, RETURN

; ----------------------------------------------------------------------
; HEXTOASC subroutine: 
; convert HEX digit to ASCII HEX digit then send digit to terminal IF it's not a leading zero
; ----------------------------------------------------------------------
HEXTOASC:BNE  HNONZERO              ; GOTO HNONZERO IF HEX digit <> 0
         CPY  #$00                  ; ELSE, GOTO HTADONE IF no non-zero digits have been processed 
         BEQ  HTADONE               ; (supress output of leading zeros)
HNONZERO:INY                        ; ELSE, indicate that a non-zero digit has been processed (Y REGISTER <> 0) 
         PHA                        ; Save HEX digit on STACK
         SEC
         SBC  #$0A
         BCS  HEXDIGIT              ; GOTO HEXDIGIT IF digit > 9
         PLA                        ; ELSE, restore digit from STACK
         ADC  #$30                  ; Add ASCII "0" to digit:convert $00 through $09 to ASCII "0" through "9"
         BCC  SENDIGIT              ; GOTO SENDIGIT:always branch
HEXDIGIT:CLC
         PLA                        ; Restore digit from STACK
         ADC  #$37                  ; Add ASCII "7" to digit:convert $0A through $0F to ASCII "A" through "F"
SENDIGIT:JSR  COUT                  ; Send converted digit to terminal
HTADONE: RTS                        ; Done HEXTOASC subroutine, RETURN

; ----------------------------------------------------------------------
; HEXTOBCD subroutine: convert a 1-8 digit HEX value to a 1-10 digit BCD value.
; Call with 8 digit (4 byte) HEX value in HEX0AND1(MSB) through HEX6AND7(LSB).
; Returns with 10 digit (5 byte) BCD result in DEC0AND1(MSB) through DEC8AND9(LSB)
; HPHANTOM is a 16 bit address used to reference an 8 bit zero-page address.
; (HEXTOBCD needs LDA $hh,Y (an invalid instruction) so we use LDA $00hh,Y instead)
; This address is not written-to nor read-from in the HEXTOBCD subroutine.
; The address is the zero-page memory location immediatly below the HEX0AND1 variable
; HEX value input buffer:
; HEX0AND1 Two most significant HEX digits
; HEX2AND3
; HEX4AND5
; HEX6AND7 Two least significant HEX digits
; BCD value output buffer (BCD accumulator):
; DEC0AND1 ; Two most significant BCD digits
; DEC2AND3
; DEC4AND5
; DEC6AND7
; DEC8AND9 ; Two least significant BCD digits
; ----------------------------------------------------------------------
HEXTOBCD:LDA  #$00                  ; Initialize (zero) output buffer. This is a 10 digit (5 byte) BCD accumulator
         STA  DEC0AND1
         STA  DEC2AND3
         STA  DEC4AND5
         STA  DEC6AND7
         STA  DEC8AND9
         LDY  #$04                  ; Initialize HEX input buffer byte MINDEX:point to address minus 1 of LSB
         LDX  #$04                  ; Initialize multiplicand table MINDEX:point to LSB of lowest multiplicand
DECLOOP: LDA  HPHANTOM,Y            ; Read MINDEXed byte from input buffer:Y REGISTER MINDEX always > 0 here 
         AND  #$0F                  ; Zero the high digit
         JSR  MULTIPLY              ; Multiply low digit
         INX                        ; Add 5 to multiplicand table MINDEX:point to LSB of next higher multiplicand
         INX
         INX
         INX
         INX
         LDA  HPHANTOM,Y            ; Read MINDEXed byte from input buffer:Y REGISTER MINDEX always > 0 here
         LSR  A                     ; Shift high digit to low digit, zero high digit
         LSR  A
         LSR  A
         LSR  A
         JSR  MULTIPLY              ; Multiply digit
         INX                        ; Add 5 to multiplicand table MINDEX:point to LSB of next higher multiplicand
         INX
         INX
         INX
         INX
         DEY                        ; Decrement HEX input buffer byte MINDEX 
         BNE  DECLOOP               ; LOOP back to DECLOOP IF byte MINDEX <> 0:there are more bytes to process
         RTS                        ; ELSE, done HEXTOBCD subroutine, RETURN

; ----------------------------------------------------------------------
; Multiply MINDEXed multiplicand by digit in ACCUMULATOR
; ----------------------------------------------------------------------
MULTIPLY:JSR  SAVREGS               ; Save A,X,Y REGISTERS on STACK
         SED                        ; Switch processor to BCD arithmatic mode
         TAY                        ; Copy digit to Y REGISTER:multiplier loop counter 
HMLTLOOP:CPY  #$00
         BNE  HDOADD                ; GOTO HDOADD IF multiplier loop counter <> 0
         CLD                        ; ELSE, switch processor to BINARY arithmatic mode
         JSR  RESREGS               ; Pull A,X,Y REGISTERS from STACK
         RTS                        ; Done MULTIPLY subroutine, RETURN
; Add MINDEXed multiplicand to BCD accumulator (output buffer)
HDOADD:  CLC
         LDA  HMULTAB,X             ; Least significant byte of MINDEXed multiplicand
         ADC  DEC8AND9              ; Least significant byte of BCD accumulator
         STA  DEC8AND9
         LDA  HMULTAB-1,X
         ADC  DEC6AND7
         STA  DEC6AND7
         LDA  HMULTAB-2,X
         ADC  DEC4AND5
         STA  DEC4AND5
         LDA  HMULTAB-3,X
         ADC  DEC2AND3
         STA  DEC2AND3
         LDA  HMULTAB-4,X           ; Most significant byte of MINDEXed multiplicand
         ADC  DEC0AND1              ; Most significant byte of BCD accumulator
         STA  DEC0AND1
         DEY                        ; Decrement multiplier loop counter
         JMP  HMLTLOOP ; LOOP back to HMLTLOOP
; BCD multiplicand table:
HMULTAB: db $00, $00, $00, $00, $01 ; BCD weight of least significant HEX digit  
         db $00, $00, $00, $00, $16
         db $00, $00, $00, $02, $56
         db $00, $00, $00, $40, $96
         db $00, $00, $06, $55, $36
         db $00, $01, $04, $85, $76
         db $00, $16, $77, $72, $16
         db $02, $68, $43, $54, $56 ; BCD weight of most significant HEX digit

; ----------------------------------------------------------------------
; INCMINDEX subroutine: increment 16 bit system variable MINDEX,MINDEXH
; ----------------------------------------------------------------------
INCMINDEX:CLC
         LDA  MINDEX
         ADC  #$01
         STA  MINDEX
         LDA  MINDEXH
         ADC  #$00
         STA  MINDEXH
         RTS                        ; Done INCMINDEX subroutine, RETURN

; ----------------------------------------------------------------------
; MEMDMP subroutine: Send a formatted ASCII or HEX memory dump to terminal 
; ----------------------------------------------------------------------
MEMDMP:  STA  MTEMP3                 ; Store dump command type:0 = HEX dump, non-0 = ASCII dump  
         JSR  DMPGR                 ; Send address offsets to terminal 
         JSR  CROUT                 ; Send CR,LF to terminal
         JSR  GLINE                 ; Send horizontal line to terminal
         JSR  CROUT                 ; Send CR,LF to terminal
         LDA  SCNT                  ; GOTO NEWADR IF an address was entered:SCNT <> 0
         BNE  NEWADR
         LDA  MTEMP2                 ; ELSE, point to next consecutive memory page
         STA  MINDEX                 ; address saved during last memory dump
         LDA  MTEMP2H
         STA  MINDEXH
         JMP  DLINE
NEWADR:  JSR  IN4                   ; Convert keystrokes in INBUFF thru INBUFF + 3 to binary, result in MINDEX,MINDEXH
DLINE:   JSR  SPC4                  ; Send 4 [SPACE] to terminal
         JSR  DOLLAR                ; Send "$" to terminal
         JSR  PRINDX                ; Send line base address to terminal
         JSR  SPC4                  ; Send 4 [SPACE] to terminal
         LDY  #$00                  ; Initialize line byte counter
         STY  IDY
GETBYTE: LDA  (MINDEX),Y             ; Read MINDEXed byte 
         LDX  MTEMP3                 ; GOTO DUMPH IF MTEMP3 = 0:HEX value output was r=ested 
         BEQ  DUMPH
         JSR  PRASC                 ; ELSE, Display byte as a printable ASCII character, send "." IF not printable
         JSR  SPC                   ; Send [SPACE] to terminal
         JMP  DUMPA                 ; GOTO DUMPA
DUMPH:   JSR  PRBYTE                ; Display byte as a HEX value
DUMPA:   JSR  SPC2                  ; Send 2 [SPACE] to terminal
         INC  IDY                   ; Increment line byte counter
         LDY  IDY
         CPY  #$10
         BNE  GETBYTE               ; LOOP back to GETBYTE IF < $10 bytes have been displayed
         JSR  CROUT                 ; ELSE, send CR,LF to terminal
         CLC                        ; Add $10 to line base address, save result in
         LDA  MINDEX                ; MINDEX,MINDEXH and MTEMP2,MTEMP2H
         ADC  #$10
         STA  MINDEX
         STA  MTEMP2
         BCC  ENDUMP                ; GOTO ENDUMP IF low byte ADD did not produce a CARRY
         INC  MINDEXH                ; ELSE, increment high byte
         LDA  MINDEXH
         STA  MTEMP2H
ENDUMP:  INC  IDX                   ; Increment line counter
         LDX  IDX
         CPX  #$10                  ; LOOP back to DLINE IF < $10 lines have been displayed
         BNE  DLINE
XITDMP:  JSR  GLINE                 ; ELSE, Send horizontal line to terminal
; DMPGR subroutine: Send address offsets to terminal
DMPGR:   JSR  CROUT
         JSR  SPC4                  ; Send 4 [SPACE] to terminal
         JSR  PUTSTR
         db  "adrs+",0
         JSR  SPC4                  ; Send 4 [SPACE] to terminal
         LDX  #$00                  ; Initialize line counter
         STX  IDX
MDLOOP:  TXA                        ; Send "00" thru "0F", separated by 2 [SPACE], to terminal  
         JSR  PRBYTE
         JSR  SPC2
         INX
         CPX  #$10
         BNE  MDLOOP
         RTS           ; Done DMPGR or MEMDMP subroutine, RETURN

; ----------------------------------------------------------------------
; PRASC subroutine: Send byte in ACCUMULATOR to terminal IF it is a printable ASCII character,
; ELSE, send "." to terminal. Printable ASCII byte values = $20 through $7E
; PERIOD subroutine: Send "." to terminal
; ----------------------------------------------------------------------
PRASC:   CMP  #$7F
         BCS  PERIOD                ; GOTO PERIOD IF byte = OR > $7F
         CMP  #$20
         BCS  ASCOUT                ; ELSE, GOTO ASCOUT IF byte = OR > $20   
PERIOD:  LDA  #$2E                  ; ELSE, load ACCUMULATOR with "."
ASCOUT:  JMP  COUT                  ; Send byte in ACCUMULATOR to terminal then done PRASC subroutine, RETURN 

; ----------------------------------------------------------------------
; PRBYTE subroutine: Send 2 ASCII HEX digits to terminal which represent value in ACCUMULATOR
; ----------------------------------------------------------------------
PRBYTE:  JSR  SAVREGS               ; Save ACCUMULATOR, X,Y REGISTERS on STACK
         JSR  BN2ASC                ; Convert byte in ACCUMULATOR to 2 ASCII HEX digits
         JSR  COUT                  ; Send most significant HEX digit to terminal
         TYA
         JSR  COUT                  ; Send least significant HEX digit to terminal
         JSR  RESREGS               ; Restore ACCUMULATOR, X,Y REGISTERS from STACK
         RTS                        ; Done PRBYTE subroutine, RETURN

; ----------------------------------------------------------------------
; PRINDX subroutine: Send 4 ASCII HEX digits to terminal which represent value in
; 16 bit variable MINDEX,MINDEXH
; ----------------------------------------------------------------------
PRINDX:  PHA                        ; Save ACCUMULATOR
         LDA  MINDEXH                ; Display MINDEX high byte
         JSR  PRBYTE
         LDA  MINDEX                 ; Display MINDEX low byte
         JSR  PRBYTE
         PLA                        ; Restore ACCUMULATOR
         RTS                        ; Done PRINDX subroutine, RETURN

; ----------------------------------------------------------------------
; READ subroutine: Read from address pointed to by MINDEX,MINDEXH
; ----------------------------------------------------------------------
MREAD:   LDY  #$00
         LDA  (MINDEX),Y
         RTS                        ; Done READ subroutine, RETURN

; ----------------------------------------------------------------------
; RDCHAR subroutine: Wait for a keystroke then clear bit 7 of keystroke value.
; IF keystroke is a lower-case alphabetical, convert it to upper-case
; ----------------------------------------------------------------------
RDCHAR:  JSR  CHIN                  ; get keystroke input from terminal
         AND  #$7F                  ; Clear bit 7 of keystroke value
         CMP  #$61
         BCC  AOK                   ; GOTO AOK IF keystroke value < $61: a control code, upper-case alPHA or numeral
         SEC                        ; ELSE, subtract $20: convert to upper-case
         SBC  #$20
AOK:     RTS                        ; Done RDCHAR subroutine, RETURN

; ----------------------------------------------------------------------
; The following subroutine (RDLINE) is derivative of published material:
; (pp. 418-424)
; "6502 assembly language subroutines"
; By Lance A. Leventhal, Winthrope Saville
; copyright 1982 by McGraw-Hill
; ISBN:0-931988-59-4
; RDLINE subroutine: Setup a keystroke input buffer then store keystrokes in buffer
; until [RETURN] key it struck. Lower-case alPHAbeticals are converted to upper-case.
; Call with:
; ACCUMULATOR = buffer address high byte
; Y REGISTER  = buffer address low byte
; X REGISTER  = buffer length
; IF variable LOKOUT = $00 then allow all keystrokes.
; IF variable LOKOUT = $FF then allow only ASCII DECIMAL numeral input:0123456789 
; IF variable LOKOUT = $01 through $FE then allow only valid ASCII HEX numeral input:0123456789ABCDEF
; [BACKSPACE] key removes keystrokes from buffer.
; [ESCAPE] key aborts RDLINE then re-enters monitor. 
; ----------------------------------------------------------------------
RDLINE:  STA  BUFADRH               ; Store buffer address high byte
         STY  BUFADR                ; Store buffer address low byte
         STX  BUFLEN                ; Store buffer length   
         LDA  #$00                  ; Initialize RDLINE buffer MINDEX:# of keystrokes stored in buffer = 0
         STA  BUFIDX
RDLOOP:  JSR  RDCHAR                ; R=est keystroke input from terminal, convert lower-case AlPHA. to upper-case
         CMP  #$1B                  ; GOTO NOTESC IF keystroke <> [ESCAPE]
         BNE  NOTESC
         JMP  NMON                  ; ELSE, abort RDLINE, GOTO NMON:re-enter monitor
; ****Replace above JMP NMON with NOP, NOP, NOP to DISABLE********************************************************
; [ESCAPE] key function during RDLINE keystroke input,
; including HEXIN subroutine digit input
; This will prevent application users from inadvertently
; entering SyMon monitor****
; ----------------------------------------------------------------------
NOTESC:  CMP  #$0D                  ; GOTO EXITRD IF keystroke = [RETURN]
         BEQ  EXITRD
         CMP  #$08                  ; ELSE, GOTO BACK IF keystroke = [BACKSPACE]
         BEQ  BACK
         LDX  LOKOUT                ; ELSE, GOTO FULTST IF variable LOKOUT = $00:keystroke filter is disabled 
         BEQ  FULTST
         CMP  #$30                  ; ELSE, filter enabled, GOTO INERR IF keystroke value < $30:value < ASCII "0"
         BCC  INERR
         CMP  #$47                  ; ELSE, GOTO INERR IF keystroke = OR > $47:value = OR > ASCII "G" ("F" + 1)
         BCS  INERR
         CMP  #$41                  ; ELSE, GOTO DECONLY IF keystroke = OR > $41:value = OR > ASCII "A"
         BCS  DECONLY
         BCC  DECTEST               ; ELSE, GOTO DECTEST:keystroke < $41:value < ASCII "A"
DECONLY: CPX  #$FF                  ; GOTO FULTST IF LOKOUT variable <> $FF:ASCII DECIMAL digit filter disabled
         BNE  FULTST
DECTEST: CMP  #$3A                  ; ELSE, DECIMAL filter enabled, GOTO INERR IF keystroke = OR > $3A:
         BCS  INERR                 ; value = OR > ASCII ":" ("9" + 1)
FULTST:  LDY  BUFIDX                ; ELSE, GOTO STRCH IF BUFIDX <> BUFLEN:buffer is not full
         CPY  BUFLEN
         BCC  STRCH
INERR:   JSR  BEEP                  ; ELSE, Send [BELL] to terminal
         BNE  RDLOOP                ; LOOP back to RDLOOP:always branch
STRCH:   STA  (BUFADR),Y            ; Store keystroke in buffer
         JSR  COUT                  ; Send keystroke to terminal
         INC  BUFIDX                ; Increment buffer MINDEX
         BNE  RDLOOP                ; LOOP back to RDLOOP:always branch
EXITRD:  LDX  BUFIDX                ; Copy keystroke count to X-REGISTER
         RTS                        ; Done RDLINE subroutine, RETURN
; Perform a destructive [BACKSPACE]
BACK:    LDA  BUFIDX                ; GOTO BSERROR IF buffer is empty
         BEQ  BSERROR
         DEC  BUFIDX                ; ELSE, decrement buffer MINDEX
         JSR  BSOUT                 ; Send [BACKSPACE] to terminal
         JSR  SPC                   ; Send [SPACE] to terminal
         JSR  BSOUT                 ; Send [BACKSPACE] to terminal
         BNE  RDLOOP                ; LOOP back to RDLOOP:always branch
BSERROR: JSR  BEEP                  ; Send [BELL] to terminal
         BEQ  RDLOOP                ; LOOP back to RDLOOP:always branch

; ----------------------------------------------------------------------
; RESREGS subroutine: restore ACCUMULATOR, X,Y REGISTERS from the copy previously saved
; on the STACK by the SAVREGS subroutine
; ----------------------------------------------------------------------
RESREGS: PLA                        ; Pull RTS RETURN address high byte from STACK
         STA  RT1                   ; Save RTS RETURN address high byte to memory
         PLA                        ; Pull RTS RETURN address low byte from STACK
         STA  RT2                   ; Save RTS RETURN address low byte to memory
         PLA                        ; Pull saved X REGISTER from STACK
         TAX                        ; Restore X REGISTER
         PLA                        ; Pull saved Y REGISTER from STACK
         TAY                        ; Restore Y REGISTER
         PLA                        ; Pull saved ACCUMULATOR from STACK
         STA  SMTEMP                 ; Save ACCUMULATOR to memory
         LDA  RT2                   ; Read RTS RETURN address low byte from memory
         PHA                        ; Push RTS RETURN address low byte onto STACK
         LDA  RT1                   ; Read RTS RETURN address high byte from memory
         PHA                        ; Push RTS RETURN address high byte onto STACK
         LDA  SMTEMP                 ; Restore ACCUMULATOR from memory
         RTS                        ; Done RESREGS subroutine, RETURN

; ----------------------------------------------------------------------
; SAVREGS subroutine: save a copy of ACCUMULATOR, X,Y REGISTERS on the STACK.
; This is used in conjunction with the RESREGS subroutine 
; ----------------------------------------------------------------------
SAVREGS: STA  SMTEMP                 ; Save ACCUMULATOR to memory
         PLA                        ; Pull RTS RETURN address high byte from STACK
         STA  RT1                   ; Save RTS RETURN address high byte to memory
         PLA                        ; Pull RTS RETURN address low byte from STACK
         STA  RT2                   ; Save RTS RETURN address low byte to memory
         LDA  SMTEMP                 ; Restore ACCUMULATOR from memory
         PHA                        ; Push ACCUMULATOR onto STACK
         TYA                        ; Push Y REGISTER onto STACK
         PHA
         TXA                        ; Push X REGISTER onto STACK
         PHA
         LDA  RT2                   ; Read RTS RETURN address low byte from memory
         PHA                        ; Push RTS RETURN address low byte onto STACK
         LDA  RT1                   ; Read RTS RETURN address high byte from memory
         PHA                        ; Push RTS RETURN address high byte onto STACK
         LDA  SMTEMP                 ; Restore ACCUMULATOR from memory
         RTS                        ; Done SAVREGS subroutine, RETURN

; ----------------------------------------------------------------------
; SETUP subroutine: R=est HEX address input from terminal 
; ----------------------------------------------------------------------
SETUP:   JSR  COUT                  ; Send command keystroke to terminal
         JSR  SPC                   ; Send [SPACE] to terminal
; R=est a 0-4 digit HEX address input from terminal
         JSR  HEXIN4                ; result in variable MINDEX,MINDEXH
         LDA  #$3A                  ; Send ":" to terminal
         JSR  COUT
         JMP  DOLLAR                ; Send "$" to terminal then done SETUP subroutine, RETURN

; ----------------------------------------------------------------------
; SPC subroutines:Send [SPACE] to terminal 1,2 or 4 times
; ----------------------------------------------------------------------
SPC4:    JSR  SPC2                  ; Send 4 [SPACE] to terminal
SPC2:    JSR  SPC                   ; Send 2 [SPACE] to terminal
SPC:     PHA                        ; Save ACCUMULATOR
         LDA  #$20                  ; Send [SPACE] to terminal
         JSR  COUT
         PLA                        ; Restore ACCUMULATOR
         RTS                        ; Done SPC(n) subroutine, RETURN

; ----------------------------------------------------------------------
; DECIN subroutine: r=est 1 - 10 DECIMAL digit input from terminal, followed by [RETURN].
; [ESCAPE] aboRTS, [BACKSPACE] erases last keystroke. 
; Convert input to BCD and HEX then store both results as follows:
; Converted 10 digit (5 byte) BCD value will be contained in variables DEC0AND1 through DEC8AND9:
; DEC0AND1 ($E5) Two most significant BCD digits
; DEC2AND3 ($E6)
; DEC4AND5 ($E7)
; DEC6AND7 ($E8)
; DEC8AND9 ($E9) Two least significant BCD digits
; Converted 8 digit (4 byte) HEX value will be contained in variables HEX0AND1 through HEX6AND7:
; HEX0AND1 ($E1) Two most significant HEX digits
; HEX2AND3 ($E2)
; HEX4AND5 ($E3)
; HEX6AND7 ($E4) Two least significant HEX digits
; NOTE1:If a DECIMAL value greater than 4,294,967,295 ($FFFFFFFF) is entered,
; 1 or 2 asterisks (*) will be sent to the terminal following the inputted digits.
; This is to indicate that an OVRFLOW in the HEX accumulator has occured.
; (the BCDTOHEX subroutine's HEX accumulator "rolls over" to zero when that value is exceeded)
; An OVRFLOW condition does NOT affect the BCD value stored.
; NOTE2:This subroutine is not used by SyMon; it is here for user purposes, if needed.    
; ----------------------------------------------------------------------
DECIN:  JSR  DECINPUT               ; R=est 1 - 10 DECIMAL digit input from terminal
        JSR  ASCTODEC               ; Convert ASCII DECIMAL digits to BCD
        JSR  BCDTOHEX               ; Convert a 1-10 digit BCD value to a 1-8 digit HEX value
        RTS                         ; Done DECIN subroutine, RETURN
; 
; 
; ******************************
; * Monitor command processors *
; ******************************
; ----------------------------------------------------------------------
; [invalid] command:No command assigned to keystroke. This handles unassigned keystrokes
; ----------------------------------------------------------------------
ERR:     JSR  BEEP                  ; Send ASCII [BELL] to terminal
         PLA                        ; Remove normal return address because we don't 
         PLA                        ; want to send a monitor prompt in this case
         JMP  CMON                  ; GOTO CMON re-enter monitor

; ----------------------------------------------------------------------
; [A] ACCUMULATOR command:Display in HEX then change ACCUMULATOR preset/result
; ----------------------------------------------------------------------
MARG:    JSR  PUTSTR
         db  "Areg: $",0
         LDA  ACCUM                 ; Read ACCUMULATOR preset/result
         JSR  CHREG                 ; Display preset/result, r=est HEX byte input from terminal
         LDX  SCNT                  ; GOTO NCAREG IF no digits were entered
         BEQ  NCAREG
         STA  ACCUM                 ; ELSE, Write to ACCUMULATOR preset/result
NCAREG:  RTS                        ; Done MARG command, RETURN

; ----------------------------------------------------------------------
; [C] CHANGE command:Display in HEX then change the contents of a specified memory address 
; ----------------------------------------------------------------------
CHANGE:  JSR  SETUP                 ; R=est HEX address input from terminal
         JSR  MREAD                  ; Read specified address
         JSR  PRBYTE                ; Display HEX value read
CHANGEL: LDA  #$08                  ; Send 3 non-destructive [BACKSPACE] to terminal
         JSR  COUT3
; Request a HEX byte input from terminal
         JSR  HEXIN2                ; result in ACCUMULATOR, X-reg and variable SCNT = # digits entered 
         LDY  SCNT                  ; Exit CHANGE command IF no digits were entered
         BEQ  XITWR
         LDY  #$00                  ; ELSE, Store entered value at current MINDEX address
         STA  (MINDEX),Y
         CMP  (MINDEX),Y             ; GOTO CHOK IF stored value matches ACCUMULATOR
         BEQ  CHOK
         LDA  #$3C                  ; ELSE, Send "<" to terminal
         JSR  COUT
         LDA  #$3F                  ; Send "?" to terminal
         JSR  COUT
CHOK:    INC  MINDEX                 ; Increment MINDEX address
         BNE  PRNXT
         INC  MINDEXH
PRNXT:   JSR  SPC2                  ; Send 2 [SPACE] to terminal
         JSR  MREAD                  ; Read from address pointed to by MINDEX,MINDEXH
         JSR  PRBYTE                ; Display HEX value read
         JMP  CHANGEL               ; LOOP back to CHANGEL:continue CHANGE command
XITWR:   RTS                        ; Done CHANGE command, RETURN

; ----------------------------------------------------------------------
; [D] HEX DUMP command:Display in HEX the contents of 256 consecutive memory addresses 
; ----------------------------------------------------------------------
MDUMP:   JSR  SETUP                 ; R=est HEX address input from terminal
         LDA  #$00                  ; Select HEX dump mode
         JMP  MEMDMP                ; Go perform memory dump then done MDUMP command, RETURN

; ----------------------------------------------------------------------
; [F] MFILL command:Fill a specified memory range with a specified value
; ----------------------------------------------------------------------
MFILL:   JSR  PUTSTR
         db  CR,LF,"Fill start: ",0
         JSR  HEXIN4                ; R=est 4 digit HEX value input from terminal. Result in variable MINDEX,MINDEXH
         LDX  SCNT                  ; GOTO DUNFILL IF no digits were entered
         BEQ  DONEFILL 
         LDA  MINDEX                 ; ELSE, copy MINDEX,MINDEXH to MTEMP2,MTEMP2H:memory fill start address
         STA  MTEMP2
         LDA  MINDEXH
         STA  MTEMP2H
         JSR  PUTSTR
         db  CR,LF,"length: ",0
         JSR  HEXIN4                ; R=est 4 digit HEX value input from terminal. Result in variable MINDEX,MINDEXH:memory
                                    ; fill length
         LDX  SCNT                  ; GOTO DUNFILL IF no digits were entered
         BEQ  DONEFILL
         JSR  PUTSTR
         db  CR,LF, "value: ",0
         JSR  HEXIN2                ; R=est 2 digit HEX value input from terminal. Result in ACCUMULATOR:fill value
         LDX  SCNT                  ; GOTO DUNFILL IF no digits were entered
         BEQ  DONEFILL
         PHA                        ; Save fill value on STACK
         JSR  PUTSTR
         db  CR,LF,"Esc key exits, any other to proceed",0
         JSR  CHIN                  ; R=est keystroke input from terminal
         CMP  #$1B                  ; GOTO QUITFILL IF keystroke = [ESCAPE]
         BEQ  QUITFILL
         PLA                        ; ELSE, pull fill value from STACK
USERFILL:LDX  MINDEXH                ; Copy MINDEXH to X REGISTER:X = length parameter page counter
         BEQ  FILEFT                ; GOTO FILEFT IF page counter = $00
         LDY  #$00                  ; ELSE, initialize page byte address MINDEX
PGFILL:  STA  (MTEMP2),Y             ; Store fill value at MINDEXed current page address
         INY                        ; Increment page byte address MINDEX 
         BNE  PGFILL                ; LOOP back to PGFILL IF page byte address MINDEX <> $00
         INC  MTEMP2H                ; ELSE, Increment page address high byte 
         DEX                        ; Decrement page counter
         BNE  PGFILL                ; LOOP back to PGFILL IF page counter <> $00 
FILEFT:  LDX  MINDEX                 ; ELSE, copy MINDEX to X REGISTER:X = length parameter byte counter 
         BEQ  DONEFILL              ; GOTO DONEFILL IF byte counter = $00
         LDY  #$00                  ; ELSE, initialize page byte address MINDEX
FILAST:  STA  (MTEMP2),Y             ; Store fill value at MINDEXed current page address
         INY                        ; Increment page byte address MINDEX
         DEX                        ; Decrement byte counter
         BNE  FILAST                ; LOOP back to FILAST IF byte counter <> $00
DONEFILL:RTS                        ; ELSE, done MFILL command, RETURN
QUITFILL:PLA                        ; Pull saved fill value from STACK:value not needed, discard
         RTS                        ; Done MFILL command, RETURN

; ----------------------------------------------------------------------
; [G] GO command:Begin executing program code at a specified address
; ----------------------------------------------------------------------
GO:      JSR  SETUP                 ; R=est HEX address input from terminal
         JSR  CROUT                 ; Send CR,LF to terminal
         LDA  MINDEX                ; Transfer specified address to monitor command processor 
         STA  COMLO                 ; address pointer low byte
         LDA  MINDEXH
         STA  COMHI                 ; hi byte
CNTLUGO: TSX                        ; Save the monitor's STACK POINTER in memory
         STX  POINTER
; Preload all 6502 MPU registers from monitor's preset/result variables
         LDX  SREG                  ; Load STACK POINTER preset
         TXS
         LDA  PREG                  ; Load PROCESSOR STATUS REGISTER preset
         PHA
         LDA  ACCUM                 ; Load ACCUMULATOR preset
         LDX  XREG                  ; Load X-REGISTER preset
         LDY  YREG                  ; Load Y-REGISTER preset
         PLP
; Call user program code as a subroutine
         JSR  DOCOM
; Store all 6502 MPU registers to monitor's preset/result variables:store results
         PHP
         STA  ACCUM                 ; Store ACCUMULATOR result
         STX  XREG                  ; Store X-REGISTER result
         STY  YREG                  ; Store Y-REGISTER result
         PLA
         STA  PREG                  ; Store PROCESSOR STATUS REGISTER result
         TSX
         STX  SREG                  ; Store STACK POINTER result
         LDX  POINTER               ; Restore the monitor's STACK POINTER
         TXS
         CLD                        ; Put 6502 in binary arithmatic mode
         RTS                        ; Done GO command, RETURN

; ----------------------------------------------------------------------
; [H] HEXTODEC command:Convert an entered 1-to-8 digit HEX value to
; a 1-to-10 digit BCD value then display DECIMAL result  
; ----------------------------------------------------------------------
HEXTODEC:JSR  PUTSTR
         db  "HEX: $",0
         JSR  HEXINPUT              ; R=est 0 to 8 digit HEX value from terminal then convert ASCII HEX digits to HEX
         LDA  BUFIDX                ; GOTO HTDDONE IF BUXIDX = 0:no digits were entered
         BEQ  HTDDONE
         JSR  PUTSTR
         db  "  DEC: ",0
         JSR  HEXTOBCD              ; Convert 8 digit HEX value to a 10 digit BCD value
         JSR  BCDOUT                ; Convert 10 digit BCD value to 10 ASCII DECIMAL digits,
                                    ; send result to terminal 
HTDDONE: RTS                        ; Done HEXTODEC command, RETURN

; ----------------------------------------------------------------------
; [J] DECTOHEX command:Convert an entered 1-to-10 digit DECIMAL value to
; a 1-to-8 digit HEX value then display HEX result
; ----------------------------------------------------------------------
DECTOHEX:JSR  PUTSTR
         db  "DEC: ",0
         JSR  DECINPUT              ; R=est 0 to 10 digit decimal numeral input from terminal
         LDA  BUFIDX                ; GOTO DONEDEC IF BUFIDX = 0:no digits were entered
         BEQ  DONEDEC
         JSR  PUTSTR 
         db  "  HEX: ",0
         JSR  ASCTODEC              ; Convert ASCII digits to BCD
         JSR  BCDTOHEX              ; Convert BCD digits to HEX
         JSR  HEXOUT                ; Convert HEX digits to ASCII HEX digits, send result to terminal
DONEDEC: RTS                        ; Done DECTOHEX command, RETURN

; ----------------------------------------------------------------------
; [M] MOVER command:Move (copy) specified source memory area to specified destination memory area
; ----------------------------------------------------------------------
MOVER:   JSR  PUTSTR         
         db  CR,LF,"Move source: ",0
         JSR  HEXIN4                ; R=est 4 digit HEX value input from terminal. Result in variable MINDEX,MINDEXH:memory
                                    ; move source address
         LDX  SCNT                  ; GOTO QUITMV IF no digits were entered
         BNE  MOVER1  
         JMP  QUITMV
MOVER1:  STA  MTEMP3                 ; ELSE, store source address in variable MTEMP3,MTEMP3H
         LDA  MINDEXH
         STA  MTEMP3H
         JSR  PUTSTR         
         db  CR,LF,"Destination: ",0
         JSR  HEXIN4                ; R=est 4 digit HEX value input from terminal. Result in variable MINDEX,MINDEXH:memory
                                    ; move destination address
         LDX  SCNT                  ; GOTO QUITMV IF no digits were entered
         BEQ  QUITMV
         STA  MTEMP2                 ; ELSE, store destination address in variable MTEMP2,MTEMP2H
         LDA  MINDEXH
         STA  MTEMP2H
         JSR  PUTSTR         
         db  CR,LF,"Length: ",0
         JSR  HEXIN4                ; R=est 4 digit HEX value input from terminal. Result in variable MINDEX,MINDEXH:memory
                                    ; move length
         LDX  SCNT                  ; GOTO QUITMV IF no digits were entered
         BEQ  QUITMV
         JSR  PUTSTR         
         db  CR,LF, "Esc key exits, any other to proceed",0
         JSR  CHIN                  ; R=est keystroke input from terminal
         CMP  #$1B                  ; GOTO QUITMV IF keystroke = [ESCAPE]
         BEQ  QUITMV
         LDA  MTEMP2                 ; ELSE, GOTO RIGHT IF source memory area is above destination memory area
         SEC                        ; AND overlaps it (destination address - source address) < length of move
         SBC  MTEMP3
         TAX                        ; X REGISTER = destination address low byte - source address low byte
         LDA  MTEMP2H
         SBC  MTEMP3H
         TAY                        ; Y REGISTER = destination address high byte - (source address high byte + borrow)
         TXA
         CMP  MINDEX                 ; produce borrow IF (destination address low - source address low) < destination low  
         TYA
         SBC  MINDEXH                ; produce borrow IF ((destination address high - source address high) + borrow) < dest. high
         BCC  RIGHT                 ; Branch if a borrow was produced (unmoved source memory overwrite condition detected)
; Move memory block first byte to last byte 
         LDY  #$00                  ; ELSE, initialize page byte MINDEX
         LDX  MINDEXH                ; Copy length high byte to X REGISTER:page counter
         BEQ  MVREST                ; GOTO MVREST IF move length is < $100 bytes
MVPGE:   LDA  (MTEMP3),Y             ; ELSE, move (copy) byte from MINDEXed source to MINDEXed destination memory
         STA  (MTEMP2),Y
         INY                        ; Increment page byte MINDEX
         BNE  MVPGE                 ; LOOP back to MVPAGE IF page byte MINDEX <> 0:loop until entire page is moved  
         INC  MTEMP3H                ; ELSE, increment source page address
         INC  MTEMP2H                ; Increment destination page address
         DEX                        ; Decrement page counter
         BNE  MVPGE                 ; LOOP back to MVPAGE IF page counter <> 0:there are more full pages to move
                                    ; ELSE,(page byte MINDEX (Y REGISTER) = 0 at this point)
MVREST:  LDX  MINDEX                 ; Copy length parameter low byte to X REGISTER:byte counter
         BEQ  QUITMV                ; GOTO QUITMV IF byte counter = 0:there are no bytes left to move
REST:    LDA  (MTEMP3),Y             ; ELSE, move (copy) byte from MINDEXed source to MINDEXed destination memory
         STA  (MTEMP2),Y
         INY                        ; Increment page byte MINDEX
         DEX                        ; Decrement byte counter
         BNE  REST                  ; LOOP back to REST IF byte counter <> 0:loop until all remaining bytes are moved 
QUITMV:  RTS                        ; ELSE, done MOVER command, RETURN
; Move memory block last byte to first byte (avoids unmoved source memory overwrite in certain source/dest. overlap) 
RIGHT:   LDA  MINDEXH                ; Point to highest address page in source memory block
         CLC                        ; source high byte = (source high byte + length high byte) 
         ADC  MTEMP3H
         STA  MTEMP3H
         LDA  MINDEXH                ; Point to highest address page in destination memory block
         CLC                        ; destination high byte = (destination high byte + length high byte) 
         ADC  MTEMP2H
         STA  MTEMP2H
         LDY  MINDEX                 ; Copy length low byte to Y REGISTER:page byte MINDEX
         BEQ  MVPAG                 ; GOTO MVPAG IF page byte MINDEX = 0:no partial page to move
RT:      DEY                        ; ELSE, decrement page byte MINDEX
         LDA  (MTEMP3),Y             ; Move (copy) byte from MINDEXed source to MINDEXed destination memory
         STA  (MTEMP2),Y
         CPY  #$00                  ; LOOP back to RT IF page byte MINDEX <> 0:loop until partial page is moved
         BNE  RT
MVPAG:   LDX  MINDEXH                ; ELSE, copy length high byte to X REGISTER:page counter
         BEQ  QUITMV                ; GOTO QUITMV IF page counter = 0:no full pages left to move
RDEC:    DEC  MTEMP3H                ; ELSE, decrement source page address
         DEC  MTEMP2H                ; Decrement destination page address
RMOV:    DEY                        ; Decrement page byte MINDEX
         LDA  (MTEMP3),Y             ; Move (copy) byte from MINDEXed source to MINDEXed destination memory
         STA  (MTEMP2),Y
         CPY  #$00                  ; LOOP back to RMOV IF page byte MINDEX <> 0:loop until entire page is moved
         BNE  RMOV
         DEX                        ; ELSE, decrement page counter
         BNE  RDEC                  ; LOOP back to RDEC IF page counter <> 0:loop until all full pages are moved
         RTS                        ; ELSE, done MOVER command, RETURN

; ----------------------------------------------------------------------
; [P] PROCESSOR STATUS command:Display then change PPOCESSOR STATUS preset/result
; ----------------------------------------------------------------------
PRG:     JSR  PUTSTR
         db  "Processor Status: $",0
         LDA  PREG                  ; Read PROCESSOR STATUS preset/result
         JSR  CHREG                 ; Display preset/result, r=est HEX byte input from terminal
         LDX  SCNT                  ; GOTO NCREG IF no digits were entered
         BEQ  NCPREG
         STA  PREG                  ; ELSE, write entered byte to PROCESSOR STATUS preset/result
NCPREG:  RTS                        ; Done PRG command, RETURN

; ----------------------------------------------------------------------
; [R] REGISTERS command:Display contents of all preset/result memory locations
; ----------------------------------------------------------------------
PRSTAT:  JSR  CROUT                 ; Send CR,LF to terminal
         LDX  #$00                  ; Initialize register MINDEX
NXTREG:  LDA  REGARA,X              ; Send MINDEXed register character (A,X,Y,S,P) to terminal
         JSR  COUT
; Send MINDEXed preset/result HEX value to terminal
         LDA  #$3D                  ; Send "=" to terminal
         JSR  COUT
         JSR  DOLLAR                ; Send "$" to terminal
; Load ACCUMULATOR from MINDEXed preset/result
         CPX  #$00
         BNE  XR
         LDA  ACCUM                 ; ACCUMULATOR preset/result
XR:      CPX  #$01
         BNE  YR
         LDA  XREG                  ; X-REGISTER preset/result
YR:      CPX  #$02
         BNE  SR
         LDA  YREG                  ; Y-REGISTER preset/result
SR:      CPX  #$03
         BNE  PR
         LDA  SREG                  ; STACK POINTER preset/result
PR:      CPX  #$04
         BNE  XITR
         LDA  PREG                  ; PROCESSOR STATUS preset/result
XITR:    JSR  PRBYTE                ; Send preset/result value to terminal
         JSR  SPC2                  ; Send 2 [SPACE] to terminal 
         CPX  #$04                  ; Increment MINDEX then LOOP back to NXTREG IF MINDEX <> 4 
         BEQ  ARONEG
         INX
         JMP  NXTREG
ARONEG:  INX                        ; ELSE, Send "-> NEG" to terminal
         CPX  #$0B
         BEQ  BIT7
         LDA  REGARA,X
         JSR  COUT
         JMP  ARONEG
BIT7:    JSR  PRBIT                 ; Send "0" or "1"  to terminal depending on MINDEXed bit condition
MNSTR:   LDY  #$03                  ; Initialize bit name character pointer
MNCHAR:  LDA  REGARA,X              ; Send MINDEXed 3 character bit name to terminal 
         JSR  COUT
         INX
         DEY
         BNE  MNCHAR
         JSR  PRBIT                 ; Send "0" or "1" to terminal depending on MINDEXed bit condition 
         CPX  #$1D                  ; LOOP back to MNSTR IF all PROCESSOR STATUS preset/result bits
         BNE  MNSTR                 ; have not been displayed
         RTS                        ; ELSE, Done PRSTAT command, RETURN

; ----------------------------------------------------------------------
; PRBIT subroutine: Send "0" or "1" to terminal depending on condition of MINDEXed bit in
; PROCESSOR STATUS preset/result 
; ----------------------------------------------------------------------
PRBIT:   LDA  #$3A                  ; Send ":" to terminal
         JSR  COUT
         LDA  PREG                  ; Read PROCESSOR STATUS preset/result
; Select number of shifts r=ired to isolate MINDEXed bit
         CPX  #$0B
         BEQ  LS1
         CPX  #$0E
         BEQ  LS2
         CPX  #$11
         BEQ  LS4
         CPX  #$14
         BEQ  LS5
         CPX  #$17
         BEQ  LS6
         CPX  #$1A
         BEQ  LS7
; Left-shift MINDEXed bit into CARRY flag
         ASL  A                     ; Bit 0 CARRY
LS7:     ASL  A                     ; Bit 1 ZERO
LS6:     ASL  A                     ; Bit 2 INTERRUPT R=EST
LS5:     ASL  A                     ; Bit 3 DECIMAL
LS4:     ASL  A                     ; Bit 4 BREAK
         ASL  A                     ; Bit 5 (This bit is undefined in the 6502 PROCESSOR STATUS REGISTER) 
LS2:     ASL  A                     ; Bit 6 OVRFLOW
LS1:     ASL  A                     ; Bit 7 NEGATIVE
         BCS  BITSET                ; Send "0" to terminal IF bit is clear
         LDA  #$30
         BNE  RDONE
BITSET:  LDA  #$31                  ; ELSE, Send "1" to terminal
RDONE:   JSR  COUT
         JMP  SPC                   ; Send [SPACE] to terminal then done PRBIT subroutine, RETURN
REGARA:  db  "AXYSP"
         db  "-> NEGOVRBRK"
         db  "DECIRQZERCAR "

; ----------------------------------------------------------------------
; [S] STACK POINTER command:Display then change STACK POINTER preset/result
; ----------------------------------------------------------------------
SRG:     JSR  PUTSTR 
         db  "Stack pointer: $",0
         LDA  SREG                  ; Read STACK POINTER preset/result
         JSR  CHREG                 ; Display preset/result, r=est HEX bytee input from terminal
         LDX  SCNT                  ; GOTO NCSREG IF no digits were entered
         BEQ  NCSREG
         STA  SREG                  ; ELSE, write entered byte to STACK POINTER preset/result
NCSREG:  RTS                        ; Done SRG command, RETURN

; ----------------------------------------------------------------------
; [T] TEXT DUMP command:Display in ASCII the contents of 256 consecutive memory addresses 
; ----------------------------------------------------------------------
ADUMP:   JSR  SETUP                 ; R=est HEX address input from terminal
         LDA  #$01                  ; Select ASCII dump mode
         JMP  MEMDMP                ; Go perform memory dump then done ADUMP command, RETURN

; ----------------------------------------------------------------------
; [X] X-REGISTER command:Display then change X-reg preset/result
; ----------------------------------------------------------------------
XRG:     JSR  PUTSTR 
         db  "Xreg: $",0
         LDA  XREG                  ; Read X-REGISTER preset/result
         JSR  CHREG                 ; Display preset/result, r=est HEX byte input from terminal
         LDX  SCNT                  ; GOTO NCXREG IF no digits were entered
         BEQ  NCXREG
         STA  XREG                  ; ELSE, write to X-REGISTER preset/result
NCXREG:  RTS                        ; Done XRG command, RETURN

; ----------------------------------------------------------------------
; [Y] Y-REGISTER command:Display then change Y-reg preset/result
; ----------------------------------------------------------------------
YRG:     JSR  PUTSTR 
         db  "Yreg: $",0
         LDA  YREG                  ; Read Y-REGISTER preset/result
         JSR  CHREG                 ; Display preset/result, r=est HEX byte input from terminal
         LDX  SCNT                  ; GOTO NCYREG IF no digits were entered
         BEQ  NCYREG
         STA  YREG                  ; ELSE, write to Y-REGISTER preset/result
NCYREG:  RTS                        ; Done YRG command, RETURN

; ----------------------------------------------------------------------
; [?] Help with monitor commands
; ----------------------------------------------------------------------
HELP:    JSR     PUTSTR
         db     CR,LF,CR,LF
         db     " A = Examine/change the ACCUMULATOR",CR,LF
         db     " C = Examine/change one or more memory locations",CR,LF
         db     " D = Examine (in hex) the contents of 256 consecutive memory locations",CR,LF
         db     " F = Fill consecutive memory locations with a specified value",CR,LF
         db     " G = Begin program execution at a specified memory location",CR,LF
         db     " H = Convert a hex value to decimal",CR,LF
         db     " J = Convert a decimal value to hex",CR,LF
         db     " L = Load an Intel hex file into memory",CR,LF         
         db     " M = Copy a specified memory range",CR,LF
         db     " P = Examine/change the PROCESSOR STATUS",CR,LF
         db     " R = Examine the contents of all registers",CR,LF
         db     " S = Examine/change the STACK POINTER",CR,LF
         db     " T = Examine (in ASCII) the contents of 256 consecutive memory locations",CR,LF
         db     " X = Examine/change the X REGISTER",CR,LF
         db     " Y = Examine/change the Y REGISTER",CR,LF,0
         RTS
         
; ----------------------------------------------------------------------
; [CONTROL-R] Re-boot the single board computer
; ----------------------------------------------------------------------         
REBOOT:  JMP  $FFFC         

; ----------------------------------------------------------------------
; The Monitor starts here
; ----------------------------------------------------------------------
MONITOR: LDA  #$00
         STA  ACCUM                 ; ACCUMULATOR preset/result value
         STA  XREG                  ; X-REGISTER preset/result value
         STA  YREG                  ; Y-REGISTER preset/result value
         STA  PREG                  ; PROCESSOR STATUS REGISTER preset/result value
         LDA  #$7F
         STA  SREG                  ; USER program/application STACK POINTER preset/result value
         JSR  CR2                   ; Send 2 CR,LF to terminal
         
; Send monitor logon messages to terminal
         JSR  PUTSTR
         db  "6502 System Monitor",CR,LF,LF
         db  "Type '?' for help.",CR,LF,0

; ----------------------------------------------------------------------
; Monitor  command input loop 
; ----------------------------------------------------------------------
         LDA  #$00                  ; Disable ASCII HEX/DEC digit filter
         STA  LOKOUT                ; (RDLINE subroutine uses this)
NMON:    LDX  #$FF                  ; Initialize STACK POINTER
         JSR  CR2                   ; Send 2 CR,LF to terminal
         LDA  #'>'
         JSR  COUT2
CMON:    JSR  RDCHAR                ; Wait for keystroke: RDCHAR converts lower-case alpha. to UPPER-CASE  
         PHA                        ; Save keystroke to STACK
         ASL  A                     ; Multiply keystroke value by 2
         TAX                        ; Get monitor command processor address from table MONTAB
         LDA  MONTAB,X              ; using multiplied keystroke value as the MINDEX
         STA  COMLO                 ; Store selected command processor address low byte
         INX
         LDA  MONTAB,X
         STA  COMHI                 ; Store selected command processor address hi byte
         PLA                        ; Restore keystroke from STACK: some command processors send keystroke to terminal
         JSR  DOCOM                 ; Call selected monitor command processor as a subroutine
         JMP  NMON                  ; LOOP back to NMON: command has been processed, go wait for next command 
DOCOM:   JMP  (COMLO)               ; Go process command then RETURN

; ----------------------------------------------------------------------
; Monitor command jump table 
; 
; List of monitor command processor addresses. These are MINDEXed by ASCII keystroke values
; which have been filtered (lower case alpha. converted to UPPER CASE) then multiplied by 2
; ----------------------------------------------------------------------
;          command:      keystroke:        keystroke value: monitor command function:
MONTAB:  dw  ERR      ; [BREAK]              $00
         dw  ERR      ; [CNTL-A]             $01  
         dw  ERR      ; [CNTL-B]             $02
         dw  ERR      ; [CNTL-C]             $03
         dw  ERR      ; [CNTL-D]             $04
         dw  ERR      ; [CNTL-E]             $05
         dw  ERR      ; [CNTL-F]             $06
         dw  ERR      ; [CNTL-G]             $07
         dw  ERR      ; [CNTL-H],[BACKSPACE] $08
         dw  ERR      ; [CNTL-I],[TAB]       $09
         dw  ERR      ; [CNTL-J],[LINEFEED]  $0A
         dw  ERR      ; [CNTL-K]             $0B
         dw  ERR      ; [CNTL-L]             $0C
         dw  ERR      ; [CNTL-M],[RETURN]    $0D
         dw  ERR      ; [CNTL-N]             $0E
         dw  ERR      ; [CNTL-O]             $0F
         dw  ERR      ; [CNTL-P]             $10
         dw  ERR      ; [CNTL-Q]             $11
         dw  REBOOT   ; [CNTL-R]             $12
         dw  ERR      ; [CNTL-S]             $13
         dw  ERR      ; [CNTL-T]             $14
         dw  ERR      ; [CNTL-U]             $15
         dw  ERR      ; [CNTL-V]             $16
         dw  ERR      ; [CNTL-W]             $17
         dw  ERR      ; [CNTL-X]             $18
         dw  ERR      ; [CNTL-Y]             $19
         dw  ERR      ; [CNTL-Z]             $1A
         dw  ERR      ; [CNTL-[],[ESCAPE]    $1B
         dw  ERR      ; [CNTL-\]             $1C
         dw  ERR      ; [CNTL-]]             $1D
         dw  ERR      ;                      $1E
         dw  ERR      ;                      $1F
         dw  ERR      ; [SPACE]              $20
         dw  ERR      ; !                    $21
         dw  ERR      ; "                    $22
         dw  ERR      ; #                    $23
         dw  ERR      ; $                    $24
         dw  ERR      ; %                    $25
         dw  ERR      ; &                    $26
         dw  ERR      ; '                    $27
         dw  ERR      ; (                    $28
         dw  ERR      ; )                    $29
         dw  ERR      ; *                    $2A
         dw  ERR      ; +                    $2B
         dw  ERR      ; ,                    $2C
         dw  ERR      ; -                    $2D
         dw  ERR      ; .                    $2E
         dw  ERR      ; /                    $2F
         dw  ERR      ; 0                    $30
         dw  ERR      ; 1                    $31
         dw  ERR      ; 2                    $32
         dw  ERR      ; 3                    $33
         dw  ERR      ; 4                    $34
         dw  ERR      ; 5                    $35
         dw  ERR      ; 6                    $36
         dw  ERR      ; 7                    $37
         dw  ERR      ; 8                    $38
         dw  ERR      ; 9                    $39
         dw  STARTDL  ; :                    $3A  start Intel Hex file download
         dw  ERR      ; ;                    $3B
         dw  ERR      ; <                    $3C
         dw  ERR      ; =                    $3D
         dw  ERR      ; >                    $3E
         dw  HELP     ; ?                    $3F  Help on monitor commands
         dw  ERR      ; @                    $40
         dw  MARG     ; A                    $41  Examine/change ACCUMULATOR preset/result
         dw  ERR      ; B                    $42
         dw  CHANGE   ; C                    $43  Examine/change a memory location's contents
         dw  MDUMP    ; D                    $44  HEX dump from specified memory address
         dw  ERR      ; E                    $45
         dw  MFILL    ; F                    $46  Fill a specified memory range with a specified value
         dw  GO       ; G                    $47  Begin program code execution at a specified address
         dw  HEXTODEC ; H                    $48  Convert an entered HEX value to a BCD value, display result
         dw  ERR      ; I                    $49
         dw  DECTOHEX ; J                    $4A  Convert an entered decimal value to a HEX value, display result
         dw  ERR      ; K                    $4B
         dw  HEXDNLD  ; L                    $4C  Intel Hex file download
         dw  MOVER    ; M                    $4D  Copy a specified memory range to a specified target address
         dw  ERR      ; N                    $4E
         dw  ERR      ; O                    $4F
         dw  PRG      ; P                    $50  Examine/change PROCESSOR STATUS REGISTER preset/result
         dw  ERR      ; Q                    $51
         dw  PRSTAT   ; R                    $52  Display all preset/result contents
         dw  SRG      ; S                    $53  Examine/change STACK POINTER preset/result
         dw  ADUMP    ; T                    $54  ASCII text dump from specified memory address
         dw  ERR      ; U                    $55
         dw  ERR      ; V                    $56
         dw  ERR      ; W                    $57 
         dw  XRG      ; X                    $58  Examine/change X-REGISTER preset/result
         dw  YRG      ; Y                    $59  Examine/change Y-REGISTER preset/result
         dw  ERR      ; Z                    $5A
         dw  ERR      ; [                    $5B
         dw  ERR      ; \                    $5C
         dw  ERR      ; ]                    $5D
         dw  ERR      ; ^                    $5E
         dw  ERR      ; _                    $5F
         dw  ERR      ; `                    $60
       

; STARTUP AND SERIAL I/O ROUTINES ===========================================================
; BY G. SEARLE 2013 =========================================================================
ACIA            equ $A000
ACIAControl     equ ACIA+0
ACIAStatus      equ ACIA+0
ACIAData        equ ACIA+1
VIA             equ $B000


                org $FF00
               
Reset:          LDX     #STACK_TOP
                TXS
    
; Initialize VIA port B as outputs to control LEDs    
                LDA #$FF
                STA VIA+2       ; Program DDRB for all port B pins as outputs
                LDA #$00
                STA VIA         ; Clear all port B outputs to turn off the blue LEDs
                
; initialize 6850 ACIA                
                LDA #$95		; Set ACIA baud rate, word size and Rx interrupt (to control RTS)
                STA	ACIAControl

; Display startup message
                LDY #0
ShowStartMsg:   LDA	StartupMessage,Y
                BEQ	WaitForKeypress
                JSR	MONCOUT
                INY
                BNE	ShowStartMsg

; Wait for a [M]onitor, [C]old start or [W]arm start selection
WaitForKeypress:JSR	MONRDKEY
                BCC	WaitForKeypress
                AND	#$DF			; Make upper case
                CMP #'M'
                BEQ StartMonitor
                CMP	#'W'
                BEQ	WarmStart
                CMP	#'C'
                BEQ ColdStart
                JMP	Reset

ColdStart:      JMP COLD_START  ; BASIC cold start
WarmStart:      JMP	RESTART		; BASIC warm start
StartMonitor:   JMP MONITOR     ; 6502 monitor

MONCOUT:        PHA
SerialOutWait:  LDA	ACIAStatus
                AND	#2
                CMP	#2
                BNE	SerialOutWait
                PLA
                STA	ACIAData
                RTS

; modified to convert lower case to UPPER CASE for BASIC - jsl 12/28/2021
MONRDKEY:       LDA	ACIAStatus
                AND	#1
                CMP	#1
                BNE	NoDataIn
                LDA	ACIAData
                AND #$7F                  ; clear bit 7 of the character
                CMP #'a'
                BCC MONRDKEY1             ; branch if the character < 'a'
                CMP #'z'+1
                BCS MONRDKEY1             ; branch if the character >= '{'
                SEC           
                SBC #$20                  ; else, convert the character to upper case          
MONRDKEY1:      SEC		                  ; set carry to indicate character available
                RTS
                
NoDataIn:       CLC		                  ; clear carry to indicate no character pressed
                RTS

MONISCNTC:      JSR	MONRDKEY
                BCC	NotCTRLC                ; If no key pressed then exit
                CMP	#3
                BNE	NotCTRLC                ; if CTRL-C not pressed then exit
                SEC		                    ; Carry set if control C pressed
                RTS
                
NotCTRLC:       CLC		                    ; Carry clear if control C not pressed
                RTS

StartupMessage: db   CR,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF,LF
                db   "6502 Single Board Computer",CR,LF,LF
                db	"[M] 6502 Monitor",CR,LF
                db   "[C] BASIC Cold Start",CR,LF
                db   "[W] BASIC Warm Start",CR,LF,LF
                db   "M/C/W?",CR,LF,0

LOAD:
SAVE:           RTS
	

                org $FFFA
                
                dw	Reset		; NMI 
                dw	Reset		; RESET 
                dw	Reset		; IRQ 

