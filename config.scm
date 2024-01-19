((compiler gcc)
 (source-dirs ./src)
 (source-ignore include)
 (output-file ./build/main)
 (features
    -fPIC -pthread)
 (libraries
    guile-3.0 gc pthread dl m GL GLEW SDL2)
 (include-paths
    src/include
    /usr/include/guile/3.0))
