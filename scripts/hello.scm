; (throw 'load-error "this is a test of exception handling")

(use-modules (ice-9 textual-ports)
             (srfi srfi-4)
             ((gl) #:prefix gl:)
             ((sdl) #:prefix sdl:))

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
           0.0   -50.0
           50.0   50.0
          -50.0   50.0 ))
        (vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer gl:+array-buffer+ vbo)
    (gl:buffer-data gl:+array-buffer+ verts gl:+static-draw+)
    (gl:vertex-attrib-pointer 0 2 gl:+float+ #f (* 2 4))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer gl:+array-buffer+ 0)
    (gl:bind-vertex-array 0)
    vao))

(define u-resolution
  (begin
    (gl:use-program program)
    (gl:get-uniform-location program "uResolution")))

(define u-location
  (begin
    (gl:use-program program)
    (gl:get-uniform-location program "uLocation")))


(define resolution
  (let ((res-i (sdl:get-window-size)))
    (f32vector (exact->inexact (s32vector-ref res-i 0))
               (exact->inexact (s32vector-ref res-i 1)))))
(define location (f32vector 0.0 0.0))
(define velocity (f32vector 2.0 2.0))

(define (update)
  (let ((res-i (sdl:get-window-size)))
    (f32vector-set! resolution 0 (exact->inexact (s32vector-ref res-i 0)))
    (f32vector-set! resolution 1 (exact->inexact (s32vector-ref res-i 1))))

  (cond
    ((> (f32vector-ref location 0) (f32vector-ref resolution 0))
      (begin
        (f32vector-set! location 0 (f32vector-ref resolution 0))
        (f32vector-set! velocity 0 (- (f32vector-ref velocity 0)))))
    ((< (f32vector-ref location 0) 0.0)
      (begin
        (f32vector-set! location 0 0.0)
        (f32vector-set! velocity 0 (- (f32vector-ref velocity 0))))))

  (cond
    ((> (f32vector-ref location 1) (f32vector-ref resolution 1))
      (begin
        (f32vector-set! location 1 (f32vector-ref resolution 1))
        (f32vector-set! velocity 1 (- (f32vector-ref velocity 1)))))
    ((< (f32vector-ref location 1) 0.0)
      (begin
        (f32vector-set! location 1 0.0)
        (f32vector-set! velocity 1 (- (f32vector-ref velocity 1))))))

  (f32vector-set! location 0 (+ (f32vector-ref location 0) (f32vector-ref velocity 0)))
  (f32vector-set! location 1 (+ (f32vector-ref location 1) (f32vector-ref velocity 1))))
  
(define (render)
  (gl:clear-color 1.0 0.0 1.0 1.0)
  (gl:clear gl:+color-buffer-bit+)
  (gl:use-program program)
  (gl:uniform2fv u-resolution resolution)
  (gl:uniform2fv u-location location)
  (gl:bind-vertex-array vao)
  (gl:draw-arrays gl:+triangles+ 0 3))