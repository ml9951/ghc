libraries/pastm_PACKAGE = pastm
libraries/pastm_dist-install_GROUP = libraries
$(if $(filter pastm,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/pastm,dist-boot,0)))
$(if $(filter pastm,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/pastm,dist-install,1)))
$(if $(filter pastm,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/pastm,dist-install,2)))
