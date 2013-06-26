SYSTEM=Darwin
MACHINE=i386

compile: yao-ming.hs
	cabal-dev install

dist: dist/build/yao-ming/yao-ming
	cp dist/build/yao-ming/yao-ming distribution/yao-ming.${SYSTEM}.${MACHINE}

puppet: /etc/puppet/modules/yao-ming/files
	cp -R distribution/* /etc/puppet/modules/yao-ming/files
