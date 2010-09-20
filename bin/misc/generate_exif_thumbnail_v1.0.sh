#!/bin/bash
############################################################################################
# sidney_v - http://blog.opensyd.fr                                                        #
############################################################################################
# Script generate_exif_thumbnail_v1.0.sh                                                   #
# Licence : GNU / GPL                                                                      #
############################################################################################
# (Re)génère la miniature EXIF d'une photo ou d'un groupe de photos                        #
# Le fichier d'origine est remplacé par un nouveau contenant la miniature                  #
# Les autres données EXIF ne sont ni modifées ni supprimées                                #
# Utile pour les photos qui ont subit une rotation mais dont la miniature est restée       #
# inchangée (ex. avec Eye of GNOME)                                                        #
############################################################################################
# Dépendances                                                                              #
############################################################################################
#  - Nécessite le paquet exiftran disponible dans le dépot Universe                        #
#  - Nécessite le paquet zenity disponible dans le dépot Main                              #
#  => sudo apt-get install exiftran zenity                                                 #
############################################################################################
# Bug connu :                                                                              #
# Problème lorsque les entêtes ne sont pas de type image/jpeg ET avec au moins 1 espace    #
############################################################################################
# 21/08/09 - v1.0 - Création                                                               #
############################################################################################

## Variables
nb_param="$#" # nb de paramètres passés au script
erreur=0 # initialisation du compteur d'erreurs totale

## Fonction génération des miniatures
generate_exif_thumbnail ()
{
for parametre in "$@"; # pour chaque fichier selectionné
        do # faire
        		name="$parametre" # on définie le nom du fichier actuel

        		if [ `file -ib "$name"` == "image/jpeg" ] ; then # on test que le fichier est bien une image jpeg
        		echo "$name Type MIME image/jpeg .... OK"
        		exiftran -g "$name" -o "${name}.tmp" && cp "${name}.tmp" "$name" && rm -f "${name}.tmp"
        		
				# on (re)génère la miniature dans un nouveau fichier
        	    # si et seulement si la première commandes est réussie la suivante est exécutée 
        	    # on renomme le fichier temporaire avec le même nom que le fichier initial
                # si et seulement si la seconde commande est réussie la suivante est exécutée
                # on supprimer alors le fichier temporaire
                
				else
				let $[erreur += 1] # incrémentation du compteur d'erreur en ajoutant +1 pour ce fichier
                echo "$name Type MIME `file -ib $name` et non pas image/jpeg .... NOK" # si type MIME différend de image/jpeg
                fi
				
        done
		nb_renomme=$(( $nb_param - $erreur )) # création de la variable nombre de fichiers renommés
        echo "-----------------------------------------"               
        echo "Fin de traitement : $# fichiers "
        echo "-----------------------------------------"
        echo "Miniatures EXIF actualisées : $nb_renomme"
        echo "Miniatures EXIF en erreur : $erreur"
        echo "-----------------------------------------"
        echo "OK, vous pouvez fermer !"
}

generate_exif_thumbnail "$@" | zenity --text-info --title "generate_exif_thumbnail" --width=500 --height=500
