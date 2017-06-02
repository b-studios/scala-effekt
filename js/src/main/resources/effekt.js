ControlJS = {

  pure: function(a) {},

  use: function(c, f) {
    console.log(c.effect.hashCode__I())
    return {
        flatMap: function () {
            return {
                run: function() {}
            }
        }
    }
  },

  handle: function(e, init, f) {
    console.log(init)
    return f({ effect: e })
  }
}