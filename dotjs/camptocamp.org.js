// vim:ft=javascript

var Config = {
  // Liste des personnes à cacher.
  // Exemple: ['Freenours', 'Krist@f']
  authors: [],
  // Liste des forums à cacher
  forums: ['nnonces', 'Partenaires'],
  // Mettre à true pour cacher par défaut, false pour l'inverse
  hide: true,
}


/**
 * Filtrage des posts d'un ou plusieurs auteurs dans une discussion
 */
if (Config.authors.length > 0 && /viewtopic/.test(window.location.href)){
  $('<a href="#" name="showhide">Montrer/cacher les posts</a>').appendTo("#brdwelcome");
  $("a[name=showhide]").click(function() {
    $.each(Config.authors, function(index, value) {
      $("#punwrap .blockpost .postleft dt:contains("+value+")").parentsUntil("#punviewtopic").slideToggle();
    });
  });

  if (Config.hide) {
    $("a[name=showhide]").click();
  }
}


/**
 * Cache les posts d'un ou plusieurs forums dans la liste des messages non lus
 * et messages récents
 */
if (Config.forums.length > 0 && /search.php/.test(window.location.href)){
  $('<a href="#" name="showhide">Montrer/cacher les posts</a>').appendTo("#brdwelcome");
  $("a[name=showhide]").click(function() {
    $.each(Config.forums, function(index, value) {
      $("#punsearch .inbox td.tc2:contains("+value+")").parentsUntil("tbody").slideToggle();
    });
  });

  if (Config.hide) {
    $("a[name=showhide]").click();
  }
}

