all:
	echo OK

clean:
	rm -rf custom.el elpa

deps-python3:
	pip3 install -r python-mode-requirements.txt
