#!/bin/bash
TAB=`echo -e "\t"`

### t.txt is a text file with list of genes
tr '\t' '\n' < data/t.txt | awk 'NF' | sort -u > data/genes_to_add.txt

### find families for each gene from nonSGS
rm data/gene_families.txt
echo -e "gene"$'\t'"family" > data/gene_families.txt
for i in `cat data/genes_to_add.txt`
do
	#grep -w "^$i" data/hugo_families.csv >> data/gene_families.txt
	grep "^$i${TAB}" data/hugo_families.csv >> data/gene_families.txt

done

### find genes not listed in HUGO families
rm a data/genes_not_assigned_to_family.txt
for i in `cat data/genes_to_add.txt`
do
	echo -n $i "" >> a
	#grep -w -q "^$i" data/hugo_families.csv && echo 'string found' || echo 'string not found' >>a
	grep -q "^$i${TAB}" data/hugo_families.csv && echo 'string found' || echo 'string not found' >>a
	echo >> a
done
grep "string not found" a | awk '{print $1}' > data/genes_not_assigned_to_family.txt
rm a

### add full name to proteins that were not assigned to a family
rm data/genes_naf_wnames.txt
echo -e "gene"$'\t'"name" > data/genes_naf_wnames.txt
for i in `cat data/genes_not_assigned_to_family.txt`
do
    grep -w $i data/UniProt-ID_gene_mapping_Apr142016.csv >> data/genes_naf_wnames.txt
done

rm data/genes_to_add.txt data/t.txt data/genes_not_assigned_to_family.txt
