# adjust is optional. if missing, will propogate to do.art.contrast and then contrast
# contrast will use its defaults.
# formula can either be "X1:X2" or ~ X1*X2 
artcon = function(m, f, response = "art", factor.contrasts="contr.sum", method = "pairwise", 
                  interaction = FALSE, adjust)
{
  f.parsed = parse.art.con.formula(f)
  # syntax handled differently for interaction contrasts.
  if(interaction){
    art.interaction.contrast = do.art.interaction.contrast(m, f.parsed, response, factor.contrasts, method,adjust)
    art.interaction.contrast
  }
  else{
    artlm.con = artlm.con.internal(m, f.parsed, response, factor.contrasts)
    art.contrast = do.art.contrast(f.parsed, artlm.con, method, adjust)
    art.contrast
  }
}