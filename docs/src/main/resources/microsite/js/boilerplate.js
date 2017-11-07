jQuery(function () {

  $("div.language-scala.boilerplate").each(function (i, el) {
    $(el).replaceWith($('<details class="language-scala boilerplate"><summary>Boilerplate</summary>' + el.innerHTML + '</details>').show());
  });

});
