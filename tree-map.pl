map_build([], nil) :- !.
map_build([(K, V) | T], TreeMap) :- map_build(T, Res), map_put(Res, K, V, TreeMap).

max(V1, V2, V3) :- V1 > V2, !, V3 is V1.
max(V1, V2, V3) :- !, V3 is V2.

get_H(nil, 0) :- !.
get_H(node(_, _, _, _, H), H).

max_H(N1, N2, Res) :- get_H(N1, H1), get_H(N2, H2), max(H1, H2, Res).
new_H(N1, N2, H) :- max_H(N1, N2, HH), H is HH + 1.

diff(node(_, _, N1, N2, _), Res) :- get_H(N1, H1), get_H(N2, H2), Res is H1 - H2.

map_get(node(K, V, _, _, _), K, V) :- !.
map_get(node(K, V, N1, N2, _), K1, V1) :- K1 > K, !, map_get(N2, K1, V1).
map_get(node(K, V, N1, N2, _), K1, V1) :- K1 < K, !, map_get(N1, K1, V1).

node_(K, V, N1, N2, H, node(K, V, N1, N2, H)).
node_(node(K, V, _, _, H), N1, N2, node(K, V, N1, N2, H)).

map_put(nil, K, V, node(K, V, nil, nil, 1)).
map_put(node(K, V1, N1, N2, H), K, V, node(K, V, N1, N2, H)).
map_put(node(K1, V1, N1, N2, H), K, V, Res) :- K > K1, !,
        map_put(N2, K, V, Ans), new_H(Ans, N1, H2),
        node_(K1, V1, N1, Ans, H2, Res1), balance(Res1, Res).
map_put(node(K1, V1, N1, N2, H), K, V, Res) :- K < K1, !,
        map_put(N1, K, V, Ans), new_H(Ans, N2, H2),
         node_(K1, V1, Ans, N2, H2, Res1), balance(Res1, Res).

map_remove(node(K, V, N1, nil, H), K, N1) :- !.
map_remove(node(K, V, N1, N2, H), K, Res) :- !, find_and_remove_min(N2, N3, Tree),
        node_(K3, V3, _, _, _, N3), new_H(N1, Tree, HH),
        node_(K3, V3, N1, Tree, HH, N4), balance(N4, Res).
map_remove(node(K0, V0, N1, N2, H0), K, Res) :- K > K0, !,
        map_remove(N2, K, Res1), new_H(N1, Res1, HH),
        node_(K0, V0, N1, Res1, HH, Res2), balance(Res2, Res).
map_remove(node(K0, V0, N1, N2, H0), K, Res) :- K < K0, !,
        map_remove(N1, K, Res1), new_H(N2, Res1, HH),
        node_(K0, V0, Res1, N2, HH, Res2), balance(Res2, Res).
map_remove(N, _, N).

find_and_remove_min(node(K, V, nil, N, H), node(K, V, nil, N, H), N) :- !.
find_and_remove_min(node(K1, V1, N1, N2, H1), R, Res) :- find_and_remove_min(N1, R, Res2),
        new_H(Res2, N2, HH), node_(K1, V1, Res2, N2, HH, Res1), balance(Res1, Res).

balance(N, Res) :- node_(_, N1, N2, N), diff(N, -2), diff(N2, 1), !, big_rotate_left(N, Res).
balance(N, Res) :- node_(_, N1, N2, N), diff(N, -2), !, rotate_left(N, Res).
balance(N, Res) :- node_(_, N1, N2, N), diff(N, 2), diff(N1, -1), !, big_rotate_right(N, Res).
balance(N, Res) :- node_(_, N1, N2, N), diff(N, 2), !, rotate_right(N, Res).
balance(Res, Res).

rotate_left(node(K, V, N1, node(K2, V2, N3, N4, H2), H), node(K2, V2, node(K, V, N1, N3, H5), N4, H)) :- new_H(N1, N3, H5).
rotate_right(node(K, V, node(K1, V1, N3, N4, H1), N2, H), node(K1, V1, N3, node(K, V, N4, N2, H5), H)) :- new_H(N2, N4, H5).
big_rotate_left(N, Res) :- node_(_, N1, N2, N), rotate_right(N2, N3), node_(N, N1, N3, N5), rotate_left(N5, Res).
big_rotate_right(N, Res) :- node_(_, N1, N2, N), rotate_left(N1, N3), node_(N, N3, N2, N5), rotate_right(N5, Res).

map_getCeiling(node(K, V, _, _, _), K, V) :- !.
map_getCeiling(node(K, V, nil, _, _), K1, V) :- K1 < K, !.
map_getCeiling(node(K, V, N1, N2, _), K1, V2) :- K1 > K, !, map_getCeiling(N2, K1, V2).
map_getCeiling(node(K, V, N1, N2, _), K1, V3) :- K1 < K, map_getCeiling(N1, K1, V3), !.
map_getCeiling(node(K, V, N1, N2, _), K1, V) :- K1 < K, !.

map_putCeiling(N, K, V, Res) :- map_putC(N, K, V, Res), !.
map_putCeiling(N, _, _, N).

map_putC(node(K, V, N1, N2, H), K, V1, node(K, V1, N1, N2, H)) :- !.
map_putC(node(K, V, nil, N2, H), K1, V1, node(K, V1, nil, N2, H)) :- K1 < K, !.
map_putC(node(K, V, N1, N2, H), K1, V1, Res) :- K1 > K, !, map_putC(N2, K1, V1, Res1), node_(K, V, N1, Res1, H, Res).
map_putC(node(K, V, N1, N2, H), K1, V1, Res) :- K1 < K, map_putC(N1, K1, V1, Res1), !, node_(K, V, Res1, N2, H, Res).
map_putC(node(K, V, N1, N2, H), K1, V1, node(K, V1, N1, N2, H)) :- K1 < K, !.
