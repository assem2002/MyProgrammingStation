;just change the assembler to sperate the labels form insts (as it used to do).
; then you can add mulitple function that just loops on the insts list to detect the information you want.
; If you wish to remove duplicates you can filter the lists you made in terms of some specific parameters (go-to *some-label*) you would look up for duplicate some-label.