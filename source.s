# Test program with various RISC-V features

.text

# Program entry point
main:
    # Stack setup
    addi x2, x2, -16       # Allocate stack frame
    sw x1, 12(x2)          # Save return address

    # Initialize registers
    addi x10, zero, 5       # Initialize x10 with 5
    addi x11, zero, 10      # Initialize x11 with 10

    # Test branch
    beq x10, x11, skip       # This branch should not be taken
    add x11, x10, x11         # x11 = x10 + x11 = 15

skip:
    # Test jump and link
    jal x1, function       # Call function, store return address in ra

    # Cleanup and exit
    lw x1, 12(x2)          # Restore return address
    addi x2, x2, 16        # Deallocate stack frame

    # Test alignment directive
    .align 2               # Align to 4-byte boundary

function:
    # Function that adds 1 to x10 and returns
    addi x11, x11, 1         # Increment x10
    jalr zero, x1, 0       # Return to caller

# Data section
.data
.align 2
value:
    .word 0xdeadbeef       # Test word directive
    .byte 0x42             # Test byte directive