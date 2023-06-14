-- ! Error with newline character in a string
main = f "abc \n def"

f "abc\ndef" = 3
