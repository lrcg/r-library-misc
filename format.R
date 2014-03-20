# Misc personal formatting functions

pval <- function (x) format.pval(x, eps = 0.001);

## Prints `r` and `p` for `cor.test` in parentheses and formatted for LaTeX

## > print.cor.test(cor.test(c(1:10,9), c(2,1:10)))
## [1] "($r=0.97$, $p <0.001$)"

## or in Sweave:
## some text \Sexpr{print.cor.test(cor.test(x, y))} some more text
print.cor.test <- function (test.result) {
    nsmall       <- 2;
    print.format <- '($r=%.*f$, %s)';

    estimate <- format.estimate(test.result$estimate, nsmall);
    p.value  <- format.p.level(test.result$p.value);
    
    sprintf(print.format, nsmall, estimate, p.value);
}

## Formats estimate to `nsmall` decimal digits 
format.estimate <- function (estimate, nsmall) round(estimate, nsmall);


# levels of p-values with LaTeX math mode output
# input           output
# --------------- ------------
# (lax mode)
# plevel(0.11, F) "$p >0.1$"
# plevel(0.1,  F) "$p =0.1$"
# plevel(0.09, F) "$p <0.1$"
# (strict mode)
# plevel(0.1)     "$p >0.05$"
# plevel(0.051)   "$p >0.05$"
# plevel(0.05)    "$p =0.05$"
# plevel(0.049)   "$p <0.05$"
# plevel(0.011)   "$p <0.05$"
# plevel(0.01)    "$p =0.01$"
# plevel(0.009)   "$p <0.01$"
# plevel(0.0011)  "$p <0.01$"
# plevel(0.001)   "$p =0.001$"
# plevel(0.0009)  "$p <0.001$"
format.p.level <- function (x, lax = FALSE) {

    cut.off.lax       <- 0.1;
    cut.off.strict    <- 0.05;
    cut.off.stricter  <- 0.01;
    cut.off.strictest <- 0.001;
    
    print.format <- '$p %s%s$';
    
    comparitor   <- '<';         # start with the positive assumption
                                 # that x makes the cut
    greater.sign <- '>';
    equal.sign   <- '=';

    cut.off <- cut.off.strict;   # assume strict which cuts off at 0.05
    if (lax) {                   # lax mode cuts off at 0.1
        cut.off <- cut.off.lax;
    } else {                     # set stricter levels if appropriate
        if (x <= cut.off.stricter)  cut.off <- cut.off.stricter
        if (x <= cut.off.strictest) cut.off <- cut.off.strictest
    }
    
    if (x == cut.off) comparitor <- equal.sign;   # equal to cut-off point 
    if (x >  cut.off) comparitor <- greater.sign; # x didn't cut it
    
    # nicely formatted in LaTeX math mode
    sprintf(print.format, comparitor, cut.off);
}
