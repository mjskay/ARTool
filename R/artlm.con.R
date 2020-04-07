
# syntax: contrast(emmeans(artlm.con(m, "X1:X2"), ~ X1X2), method="pairwise")
artlm.con = function(m, f, response="art", factor.contrasts="contr.sum", ...)
{
  f.parsed = parse.art.con.string.formula(f)
  
  artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts,...)
  artlm.con
}