;;;; stacks.lisp

(in-package #:sheaf/tests)

(in-suite sheaf)

(defvar *assigned* (make-hash-table :test 'equal))

(defun assigned (fixture)
  "FIXTURE's lowered program with its stacks assigned, once."
  (or (gethash fixture *assigned*)
      (setf (gethash fixture *assigned*)
            (sheaf:assign-stacks (lowered fixture)))))

(defun clones (fixture name)
  "The clones in FIXTURE's assigned program of the function NAME."
  (loop :for def :being :the :hash-values :of (sheaf:lowered-defs (assigned fixture))
        :when (eq (sheaf:ir-def-origin def) (fixture-symbol fixture name))
          :collect def))

(defun constructs (def)
  (let ((nodes '()))
    (sheaf:walk-ir (lambda (node) (when (typep node 'sheaf:ir-construct) (push node nodes)))
                   (sheaf:ir-def-body def))
    nodes))

(defun calls (def)
  (let ((nodes '()))
    (sheaf:walk-ir (lambda (node) (when (typep node 'sheaf:ir-call) (push node nodes)))
                   (sheaf:ir-def-body def))
    (nreverse nodes)))

(test assigned-programs-are-well-formed
  (dolist (fixture '("counter" "closures" "flows" "recursive-data" "stacks"))
    (is-true (sheaf:check-assigned (assigned fixture)) "~a" fixture)
    (is-true (sheaf:check-lowered (assigned fixture)) "~a" fixture)))

(test assigned-programs-keep-their-results
  (is (= 51 (sheaf:run-lowered (assigned "closures"))))
  (is (= 55 (sheaf:run-lowered (assigned "flows"))))
  (is (= 21 (sheaf:run-lowered (assigned "recursive-data"))))
  (is (= 47 (sheaf:run-lowered (assigned "stacks")))))

(test raw-lisp-still-cannot-run-when-assigned
  (signals error (sheaf:run-lowered (assigned "counter"))))

(test programs-without-lists-need-no-stacks
  (is (= 0 (sheaf:lowered-stack-count (assigned "closures")))))

(test a-list-in-main-takes-one-stack
  (is (= 1 (sheaf:lowered-stack-count (assigned "flows")))))

(test stack-count-is-the-stacks-a-run-touches
  (dolist (fixture '("recursive-data" "stacks"))
    (let ((assigned (assigned fixture)))
      (is (= (sheaf:lowered-stack-count assigned)
             (nth-value 1 (sheaf:run-lowered assigned)))
          "~a" fixture))))

(test a-returned-list-is-built-in-the-callers-destination
  (let ((pairs (clones "stacks" "PAIR")))
    (is (= 2 (length pairs)) "one clone per destination")
    (dolist (pair pairs)
      (is (integerp (sheaf:ir-def-destination pair)))
      (is (null (sheaf:ir-def-pool pair)))
      (dolist (construct (constructs pair))
        (is (= (sheaf:ir-def-destination pair) (sheaf:ir-construct-stack construct)))))))

(test a-dropped-list-is-built-in-a-pool-of-its-own
  (let ((dropped (first (clones "stacks" "DROPPED"))))
    (is (null (sheaf:ir-def-destination dropped)))
    (is (integerp (sheaf:ir-def-pool dropped)))
    (is (= (sheaf:ir-def-pool dropped)
           (sheaf:ir-call-destination (find-if #'sheaf:ir-call-destination (calls dropped)))))))

(test sequential-calls-reuse-a-stack
  (is (= 1 (length (clones "stacks" "DROPPED"))))
  (let ((callees (mapcar #'sheaf:ir-call-function (calls (first (clones "stacks" "TWICE"))))))
    (is (= 2 (length callees)))
    (is (eq (first callees) (second callees)))))

(defun call-to (def fixture name)
  "The first call in DEF to a clone of the function NAME."
  (find-if (lambda (call)
             (eq (sheaf:ir-def-origin (gethash (sheaf:ir-call-function call)
                                               (sheaf:lowered-defs (assigned fixture))))
                 (fixture-symbol fixture name)))
           (calls def)))

(test a-cycle-allocates-in-one-region
  (let* ((walk (first (clones "stacks" "WALK")))
         (entry (call-to (first (clones "stacks" "MAIN")) "stacks" "WALK"))
         (inner (find-if #'sheaf:ir-call-destination (calls walk))))
    (is (null (sheaf:ir-def-pool walk)) "the call into the cycle owns the region")
    (is (integerp (sheaf:ir-call-region entry)))
    (is (= (sheaf:ir-call-region entry) (sheaf:ir-call-destination inner)))
    (is (null (sheaf:ir-call-region (call-to walk "stacks" "WALK"))))))

(test a-global-list-has-a-stack-for-good
  (let ((table (first (clones "stacks" "TABLE"))))
    (is (eq :value (sheaf:ir-def-kind table)))
    (is (integerp (sheaf:ir-def-destination table)))))

(defun tamper (fixture)
  "A fresh assignment of FIXTURE with every cell pushed to the highest stack in its program."
  (let* ((assigned (sheaf:assign-stacks (lowered fixture))))
    (maphash (lambda (name def)
               (declare (ignore name))
               (sheaf:walk-ir (lambda (node)
                                (when (and (typep node 'sheaf:ir-construct) (sheaf:ir-construct-stack node))
                                  (setf (sheaf::ir-construct-stack node)
                                        (1- (sheaf:lowered-stack-count assigned)))))
                              (sheaf:ir-def-body def)))
             (sheaf:lowered-defs assigned))
    assigned))

(test a-wrong-assignment-is-caught
  (signals error (sheaf:run-lowered (tamper "stacks"))))

(test storage-the-result-does-not-hold-stays-in-the-pool
  (is (= 12 (sheaf:run-lowered (assigned "placement"))))
  (let* ((relay (first (clones "placement" "RELAY")))
         (pair-call (call-to relay "placement" "PAIR")))
    (is (integerp (sheaf:ir-def-pool relay)))
    (is (eql (sheaf:ir-def-pool relay) (sheaf:ir-call-destination pair-call)))
    (is (not (eql (sheaf:ir-def-destination relay) (sheaf:ir-call-destination pair-call))))))

(test a-call-returning-a-view-has-no-destination
  (let ((read-second (first (clones "placement" "READ-SECOND"))))
    (is (null (sheaf:ir-call-destination (call-to read-second "placement" "SECOND-REF"))))))

(test a-value-stored-into-a-returned-array-is-built-in-the-destination
  (let ((filled (first (clones "placement" "FILLED"))))
    (is (integerp (sheaf:ir-def-destination filled)))
    (is (null (sheaf:ir-def-pool filled)))
    (is (= 3 (length (constructs filled))))
    (dolist (construct (constructs filled))
      (is (eql (sheaf:ir-def-destination filled) (sheaf:ir-construct-stack construct))))))

(test summaries-name-the-locals-a-result-holds
  (let ((summaries (sheaf:check-roots (lowered "placement"))))
    (flet ((reached (name)
             (loop :for def :being :the :hash-values :of (sheaf:lowered-defs (lowered "placement"))
                   :when (eq (sheaf:ir-def-origin def) (fixture-symbol "placement" name))
                     :return (sheaf:root-summary-reached (gethash (sheaf:ir-def-name def) summaries)))))
      (is (null (reached "RELAY")))
      (is (= 4 (length (reached "FILLED"))))
      (is (= 2 (length (reached "PAIR")))))))

;;; Growable locals of the opaque Vector type, built by hand

(defvar *vector* (sheaf::make-ir-type :vector :reference 'coalton/vector:vector '()))
(defvar *number* (sheaf::make-ir-type :number :scalar 'coalton:ufix '()))

(defun hand-built (root-result body-fn)
  "A program whose `make` returns a Vector and whose root is `main`, with the
body BODY-FN builds from a function that makes a call of `make`."
  (let ((lowered (sheaf::%make-lowered :root 'main)))
    (flet ((make-call ()
             (sheaf::make-ir-call :function 'make :args '() :type *vector*))
           (def (name result body)
             (setf (gethash name (sheaf:lowered-defs lowered))
                   (sheaf::make-ir-def name nil :function '() result body nil))))
      (def 'make *vector* (sheaf::make-ir-foreign :vars '() :form nil :type *vector*))
      (def 'main root-result (funcall body-fn #'make-call)))
    (sheaf:assign-stacks lowered)))

(defun zero ()
  (sheaf::make-ir-literal :value 0 :type *number*))

(defun root-body (assigned)
  (sheaf:ir-def-body (gethash (sheaf:lowered-root assigned) (sheaf:lowered-defs assigned))))

(test live-growable-locals-take-different-stacks
  (let ((assigned (hand-built *number*
                              (lambda (make-call)
                                (sheaf::make-ir-let
                                 :name 'v :value (funcall make-call) :type *number*
                                 :body (sheaf::make-ir-let
                                        :name 'w :value (funcall make-call) :type *number*
                                        :body (zero)))))))
    (let* ((outer (root-body assigned)) (inner (sheaf:ir-let-body outer)))
      (is (= 0 (sheaf:ir-let-stack outer)))
      (is (= 1 (sheaf:ir-let-stack inner)))
      (is (= 0 (sheaf:ir-call-destination (sheaf:ir-let-value outer))))
      (is (= 2 (sheaf:lowered-stack-count assigned))))))

(test growable-locals-in-different-branches-share-a-stack
  (let ((assigned (hand-built
                   *number*
                   (lambda (make-call)
                     (flet ((branch ()
                              (sheaf::make-ir-branch
                               :pattern (sheaf::make-ir-pwild :type *number*)
                               :body (sheaf::make-ir-let :name 'v :value (funcall make-call)
                                                         :type *number* :body (zero)))))
                       (sheaf::make-ir-match :scrutinee (zero) :type *number*
                                             :branches (list (branch) (branch))))))))
    (is (equal '(0 0) (mapcar (lambda (branch) (sheaf:ir-let-stack (sheaf:ir-branch-body branch)))
                              (sheaf:ir-match-branches (root-body assigned)))))
    (is (= 1 (sheaf:lowered-stack-count assigned)))))

(test a-returned-growable-value-is-built-in-the-destination
  (let* ((assigned (hand-built *vector*
                               (lambda (make-call)
                                 (sheaf::make-ir-let
                                  :name 'v :value (funcall make-call) :type *vector*
                                  :body (sheaf::make-ir-local :name 'v :type *vector*)))))
         (body (root-body assigned)))
    (is (null (sheaf:ir-let-stack body)))
    (is (= (sheaf:ir-def-destination (gethash (sheaf:lowered-root assigned) (sheaf:lowered-defs assigned)))
           (sheaf:ir-call-destination (sheaf:ir-let-value body))))))

(defun leaking (owner)
  "A program whose `leak` builds a list in the pool of OWNER, `leak` or `main`."
  (let* ((list-type (sheaf::make-ir-type :list :data 'list '()))
         (lowered (sheaf::%make-lowered :root 'main :stack-count 1))
         (nothing (sheaf::make-ir-construct :constructor 'nothing :args '() :type list-type :stack 0))
         (leak (sheaf::make-ir-def
                'leak nil :function '() list-type
                (sheaf::make-ir-let
                 :name 'end :value nothing :type list-type
                 :body (sheaf::make-ir-construct
                        :constructor 'link :type list-type :stack 0
                        :args (list (zero) (sheaf::make-ir-local :name 'end :type list-type))))
                nil))
         (main (sheaf::make-ir-def
                'main nil :function '() *number*
                (sheaf::make-ir-let
                 :name 'result :value (sheaf::make-ir-call :function 'leak :args '() :type list-type)
                 :type *number*
                 :body (sheaf::make-ir-match
                        :scrutinee (sheaf::make-ir-local :name 'result :type list-type)
                        :type *number*
                        :branches (list (sheaf::make-ir-branch
                                         :pattern (sheaf::make-ir-pwild :type list-type)
                                         :body (zero)))))
                nil)))
    (setf (gethash list-type (sheaf:lowered-types lowered))
          (list (sheaf::make-ir-constructor 'nothing 0 '())
                (sheaf::make-ir-constructor 'link 1 (list *number* list-type)))
          (gethash 'leak (sheaf:lowered-defs lowered)) leak
          (gethash 'main (sheaf:lowered-defs lowered)) main
          (sheaf:ir-def-pool (if (eq owner :leak) leak main)) 0)
    lowered))

(test a-cell-outliving-its-stack-is-caught
  (is (= 0 (sheaf:run-lowered (leaking :main))))
  (signals error (sheaf:run-lowered (leaking :leak))))
