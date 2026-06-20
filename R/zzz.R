# Package Load/Attach Hooks for RMediation

.onLoad <- function(libname, pkgname) {
  # Register S4 classes for S7
  S7::S4_register(ProductNormal)
  S7::S4_register(MBCOResult)

  # Register S7 methods
  S7::methods_register()

  # Register medfit integration methods if medfit is available
  .register_medfit_methods()
}
