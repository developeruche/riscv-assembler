.text
addi x2, zero, -16    
addi x3, zero, 0
addi x4, zero, 1       # Allocate stack space

sw x3, 0(x2) 
sw x3, 4(x2) 
sw x3, 8(x2) 
sw x3, 12(x2)
sw x3, 16(x2)
sw x3, 20(x2)
sw x3, 24(x2)
sw x4, 28(x2)