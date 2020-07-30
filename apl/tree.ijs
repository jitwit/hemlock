NB. array based tree functions in J

example0 =: 0 1 2 3 3 4 2 3 3 4 0 1 2 2 3 3 4 2 3 4 0 1 2 3 3 4 2 3 3 4

NB. convert depth vector into parent vector
parent =: 3 : 0
ps=. 0 #~ n =. # y
for_xy. 2 ]\ (i.n) </.~ y
do. ps=. ps y }~ x {~ <: x I. y [ 'x y' =. xy
end. ps
)

NB. simpler but quadratic parent conversion
parent_quad =: * * (i: <:@{:)\

NB. build path matrix from depth vector
path_matrix =: >./\ @: |: @: (>./\) @: (- |."0 1 i.@# ,. 0 $~ #`(>./)`:0)
