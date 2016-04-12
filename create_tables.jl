#!/usr/bin/env julia

using DataFrames

df = readtable("rawdata.csv")
jdf = join(df[df[:prepost].=="Pre", :], df[df[:prepost].=="Post", :], on=:id)

cols = names(df)[3:end]

tab = DataFrame()
for c in cols
    tdf = by(jdf, [c, symbol("$(c)_1")], f->DataFrame(n=nrow(f)))
    names!(tdf, [:initial, :final, :n])
    tdf[:topic] = c
    tab = vcat(tab, tdf)
end

writetable("procdata.csv", tab)
