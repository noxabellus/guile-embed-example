; (throw 'load-error "this is a test of exception handling")

(use-modules (ice-9 textual-ports)
             (srfi srfi-4)
             ((gl) #:prefix gl:))

(define (print-ln . args)
  (for-each display args)
  (newline))

(print-ln "hello from guile" 1 2 3)

(define program
  (let* ((vert-f (open-input-file "shaders/basic.vert"))
         (vert-sh (gl:make-shader gl:+vertex-shader+ (get-string-all vert-f)))
         (frag-f (open-input-file "shaders/basic.frag"))
         (frag-sh (gl:make-shader gl:+fragment-shader+ (get-string-all frag-f))))
    (close-port vert-f)
    (close-port frag-f)
    (gl:make-program vert-sh frag-sh)))

(define vao
  (let ((verts (f32vector
          -0.5 -0.5 0.0
           0.5 -0.5 0.0
           0.0  0.5 0.0 ))
        (vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer gl:+array-buffer+ vbo)
    (gl:buffer-data gl:+array-buffer+ verts gl:+static-draw+)
    (gl:vertex-attrib-pointer 0 3 gl:+float+ #f (* 3 4))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer gl:+array-buffer+ 0)
    (gl:bind-vertex-array 0)
    vao))

(define (render)
  (gl:clear-color 1.0 0.0 1.0 1.0)
  (gl:clear gl:+color-buffer-bit+)
  (gl:use-program program)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays gl:+triangles+ 0 3))