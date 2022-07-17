spelling : write.cmo spelling.cmo
	ocamlc -o spelling spelling.cmo write.cmo

write.cmo : write.ml spelling.cmi
	ocamlc -c write.ml

spelling.cmo : spelling.ml spelling.mli
	ocamlc -c spelling.ml

spelling.cmi : spelling.mli
	ocamlc -c spelling.mli

clean:
	rm spelling
	rm *.cmo
	rm *.cmi

