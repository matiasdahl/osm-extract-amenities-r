require('gdata')

#
#  Approximate big numbers (>1000) by dropping last n digits to zero.
#
ca_number <- function(n, digits=2) {
    assert(n < 10^8)
    return(round(n, -digits))
}
assert(ca_number(12345) == 12300)
assert(ca_number(12345, digits=1) == 12340)       # Note: round(0.5) = 0:
assert(ca_number(12999) == 13000)
assert(ca_number(12, digits=1) == 10)
assert(ca_number(123, digits=1) == 120)

#
#  Render percentages
#
render_ratio <- function(ratio) {
    with_sign <- function(str) return(paste0(str, '%'))
    return (with_sign(as.character(round(100.0 * ratio, 1))))
}
assert(render_ratio(100/100) == "100%")
assert(render_ratio(9.99/100) == "10%")
assert(render_ratio(10/100) == "10%")
assert(render_ratio(1/100) == "1%")
assert(render_ratio(0.1/100) == "0.1%")
assert(render_ratio(0.15/100) == "0.2%")
assert(render_ratio(0.009/100) == "0%")

#
#  Simplify number. Eg. `123478` -> `123k`
#
human_number_unit <- function(n, user_digits, user_units) {
    assert(user_units %in% c('k', 'M'))

    withB <- humanReadable(n, standard="SI",
                           digits = user_digits,
                           width = NULL,
                           sep = "",
                           units = paste0(user_units, 'b'))
    # renove trailing 'b' from result
    return(substr(withB, 1, nchar(withB) - 1))
}
assert(human_number_unit(123, 0, 'k') == '0k')
assert(human_number_unit(1234, 0, 'k') == '1k')
assert(human_number_unit(1234, 1, 'k') == '1.2k')
assert(human_number_unit(1234, 0, 'M') == '0M')
assert(human_number_unit(1234, 1, 'M') == '0.001M')
assert(human_number_unit(123456, 0, 'k') == '123k')
assert(human_number_unit(123456, 1, 'k') == '123.5k')
assert(human_number_unit(123456, 0, 'M') == '0M')
assert(human_number_unit(123456, 1, 'M') == '0.1M')

human_number <- function(n, user_units = 'auto') {
    assert(n < 10^9)

    if (n < 1000) return(n)

    # 10k -> 995k
    if (n < 1000000) return(human_number_unit(n, user_digits = 1, user_units = 'k'))

    # 1.0M -> ..
    return(human_number_unit(n, user_digits = 1, user_units = 'M'))
}

assert(human_number(123) == 123)
assert(human_number(1234) == "1.2k")
assert(human_number(1999) == "2.0k")
assert(human_number(999999) == "1000.0k")
assert(human_number(1999999) == "2.0M")
assert(human_number(4000000) == "4.0M")
