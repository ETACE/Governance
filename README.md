# Governance
Here we supply the code to the paper "Governance structure, technical change and industry competition" by M. Guerini, P. Harting and M. Napoletano. The tagged code for the working paper can be found as tag WP.

The working paper can be found at this link: http://www.lem.sssup.it/WPLem/files/2020-35.pdf

# Dependencies

GCC 4.4 or higher (or other C compiler)

GSL 2.4 or higher (GNU Scientific Library)

FLAME XParser 0.17.1 (https://github.com/FLAME-HPC/xparser/archive/0.17.1.tar.gz)

FLAME Libmboard 0.3.1 (https://github.com/FLAME-HPC/libmboard/archive/0.3.1.zip)
Pre-installed FLAME libraries and model source code



# Installation of FLAME libraries on stand-alone systems and building the model

1. Prepare system requirements

Make sure to have cunit and gsl installed.

    On Debian-based systems: sudo apt-get install libcunit1 libcunit1-doc libcunit1-dev libgsl-dev.
    On macOS: brew install cunit gsl.
    On Windows (TODO: test on appveyor): TODO.

2. Install libmboard

On top of the repo root directory, download libmboard 0.3.1 (https://github.com/FLAME-HPC/libmboard/archive/0.3.1.tar.gz) and extract the tarball

    mkdir libmboard
    cd libmboard-0.3.1
    chmod +x autogen.sh
    ./autogen.sh

Remove --disable-parallel if you have mpi installed

    ./configure --prefix=$(cd ../libmboard; pwd) --disable-parallel
    make
    make install
    #back to the repo root directory
    cd ..

3. Install xparser

Download xparser 0.17.1 (https://github.com/FLAME-HPC/xparser/archive/0.17.1.tar.gz) and extract to xparser/ directory on top of the repo root directory.

    cd xparser
    make
    #back to the repo root directory
    cd ..

4. Generate makefile for the model
    
         cd xparser./xparser ../model.xml
         #back to the repo root directory
         cd ..

5. Build the model

Be sure to replace LIBMBOARD_DIR in Makefile from /usr/local to $(PWD)/libmboard. Finally, run make, which should compile everything if the previous steps are executed correctly.
