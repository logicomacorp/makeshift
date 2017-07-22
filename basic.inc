    .const basic_token_for = $81
    .const basic_token_next = $82
    .const basic_token_poke = $97
    .const basic_token_print = $99
    .const basic_token_sys = $9e
    .const basic_token_to = $a4
    .const basic_token_step = $a9
    .const basic_token_addition = $aa
    .const basic_token_subtraction = $ab
    .const basic_token_multiplication = $ac
    .const basic_token_division = $ad
    .const basic_token_and = $af
    .const basic_token_equals = $b2
    .const basic_token_int = $b5
    .const basic_token_cos = $be
    .const basic_token_sin = $bf
    .const basic_token_peek = $c2
    .const basic_token_pi = $ff

    .macro StartBasicLineWithoutToken(next_addr, line) {
        .word next_addr
        .word line
    }

    .macro StartBasicLine(next_addr, line, token) {
        StartBasicLineWithoutToken(next_addr, line)
        .byte token
    }

    .macro EndBasicLine() {
        .byte $00
    }

    .macro EndBasicProgram() {
        .byte $00, $00
    }

    .macro EmptyRealVar(name) {
        .if(name.size() == 0) {
            .error "Variable name cannot be empty"
        }
        .if(name.size() > 2) {
            .error "Variable name must be 1 or 2 characters long: " + name
        }
        .text name
        .if(name.size() == 1) {
            .byte $00
        }
        .fill 5, $00
    }

    .macro EmptyIntegerVar(name) {
        .if(name.size() == 0) {
            .error "Variable name cannot be empty"
        }
        .if(name.size() > 2) {
            .error "Variable name must be 1 or 2 characters long: " + name
        }
        .for(var i = 0; i < 2; i++) {
            .var b = $80
            .if(i < name.size()) {
                .eval b += name.charAt(i)
            }
            .byte b
        }
        .fill 5, $00
    }
