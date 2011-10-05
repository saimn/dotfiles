// vim:ft=javascript

var Config = {
  // liste des personnes à cacher
  authors: ['Freenours', 'Krist@f'],
  // liste des forums à cacher
  forums: ['nnonces', 'Partenaires'],
  // mettre à true pour cacher par défaut, false pour l'inverse
  hide: true,
}

/**
 * Filter author's posts
 */
if (/viewtopic/.test(window.location.href)){
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
 * Hide some forums in search pages
 */
if (/search.php/.test(window.location.href)){
  $('<a href="#" name="showhide">Montrer/cacher les posts</a>').appendTo("#brdwelcome");
  $("a[name=showhide]").click(function() {
    $.each(Config.forums, function(index, value) {
      $("#punsearch .inbox td.tc2:contains("+value+")").parentsUntil("#tbody").slideToggle();
    });
  });

  if (Config.hide) {
    $("a[name=showhide]").click();
  }
}


