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

(defun static-clones (fixture name)
  "The clones of NAME that build their growable values on stacks of their own, not from a bank."
  (remove-if #'sheaf:ir-def-bank (clones fixture name)))

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

;;; Growable locals of a Vector, built by hand

(defvar *number* (sheaf::make-ir-type :number :scalar 'coalton:ufix '()))
(defvar *growable* (sheaf::make-ir-type :vector :storage 'coalton/vector:vector (list *number*)))

(defun hand-built (root-result body-fn)
  "A program whose `make` returns a Vector and whose root is `main`, with the
body BODY-FN builds from a function that makes a call of `make`."
  (let ((lowered (sheaf::%make-lowered :root 'main)))
    (flet ((make-call ()
             (sheaf::make-ir-call :function 'make :args '() :type *growable*))
           (def (name result body)
             (setf (gethash name (sheaf:lowered-defs lowered))
                   (sheaf::make-ir-def name nil :function '() result body nil))))
      (def 'make *growable* (sheaf::make-ir-foreign :vars '() :form nil :type *growable*))
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
      (is (equal '(0) (sheaf:ir-let-stacks outer)))
      (is (equal '(1) (sheaf:ir-let-stacks inner)))
      (is (equal '(0) (sheaf:ir-call-growables (sheaf:ir-let-value outer))))
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
    (is (equal '((0) (0)) (mapcar (lambda (branch) (sheaf:ir-let-stacks (sheaf:ir-branch-body branch)))
                              (sheaf:ir-match-branches (root-body assigned)))))
    (is (= 1 (sheaf:lowered-stack-count assigned)))))

(test a-returned-growable-value-is-built-on-a-growable-stack-of-the-caller
  (let* ((assigned (hand-built *growable*
                               (lambda (make-call)
                                 (sheaf::make-ir-let
                                  :name 'v :value (funcall make-call) :type *growable*
                                  :body (sheaf::make-ir-local :name 'v :type *growable*)))))
         (body (root-body assigned)))
    (is (null (sheaf:ir-let-stacks body)))
    (is (null (sheaf:ir-call-destination (sheaf:ir-let-value body))))
    (is (= 1 (length (sheaf:ir-call-growables (sheaf:ir-let-value body)))))
    (is (equal (sheaf:ir-def-growables (gethash (sheaf:lowered-root assigned) (sheaf:lowered-defs assigned)))
               (sheaf:ir-call-growables (sheaf:ir-let-value body))))))

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

;;; Growable values a result holds

(defun growable-stacks-of (def)
  (sort (copy-list (sheaf:ir-def-growables def)) #'<))

(defun prim-stacks-of (def)
  (sort (mapcar #'sheaf:ir-prim-stack (allocating-prims-of def)) #'<))

(test growables-give-one-result-on-every-tier
  (let ((expected (run-on-sbcl "growables")))
    (is (= 36 expected))
    (is (= expected (sheaf:run-lowered (lowered "growables"))))
    (is (= expected (sheaf:run-lowered (assigned "growables"))))))

(test growables-are-well-formed-and-count-the-stacks-a-run-touches
  (let ((assigned (assigned "growables")))
    (is-true (sheaf:check-assigned assigned))
    (is-true (sheaf:check-lowered assigned))
    (is (= (sheaf:lowered-stack-count assigned) (nth-value 1 (sheaf:run-lowered assigned))))))

(test each-vector-a-result-holds-has-a-stack-of-its-own
  (dolist (two (static-clones "growables" "TWO"))
    (is (null (sheaf:ir-def-destination two)))
    (is (= 2 (length (remove-duplicates (sheaf:ir-def-growables two)))))
    (is (equal (growable-stacks-of two) (prim-stacks-of two)))))

(test a-vector-and-a-list-a-result-holds-are-on-different-stacks
  (dolist (pair (clones "growables" "WITH-LIST"))
    (let ((destination (sheaf:ir-def-destination pair)))
      (is (integerp destination))
      (is (= 1 (length (sheaf:ir-def-growables pair))))
      (is (not (member destination (sheaf:ir-def-growables pair))))
      (is (equal (sheaf:ir-def-growables pair) (prim-stacks-of pair)))
      (let ((cells (remove-if-not #'sheaf:ir-construct-stack (constructs pair))))
        (is (= 3 (length cells)))
        (dolist (cell cells)
          (is (eql destination (sheaf:ir-construct-stack cell))))))))

(test branches-share-a-growable-stack
  (dolist (either (clones "growables" "EITHER"))
    (is (= 1 (length (sheaf:ir-def-growables either))))
    (is (= 2 (length (allocating-prims-of either))))
    (is (equal (sheaf:ir-def-growables either) (remove-duplicates (prim-stacks-of either))))))

(test a-tail-call-passes-its-growable-stacks-on
  (dolist (relay (clones "growables" "RELAY-TWO"))
    (is (= 2 (length (sheaf:ir-def-growables relay))))
    (is (equal (sheaf:ir-def-growables relay)
               (sheaf:ir-call-growables (first (calls relay)))))))

(test a-dropped-call-owns-a-stack-for-each-growable-it-builds
  (let* ((main (first (clones "growables" "MAIN-TWO"))) (found nil))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-let) (typep (sheaf:ir-let-value node) 'sheaf:ir-call))
                       (setf found node)))
                   (sheaf:ir-def-body main))
    (is (= 2 (length (sheaf:ir-let-stacks found))))
    (is (null (sheaf:ir-call-destination (sheaf:ir-let-value found))))
    (is (equal (sheaf:ir-let-stacks found) (sheaf:ir-call-growables (sheaf:ir-let-value found))))))

(test a-recursive-builder-of-one-growable-reuses-its-clone
  (dolist (down (clones "growables" "FILLED-DOWN"))
    (is (= 1 (length (sheaf:ir-def-growables down))))
    (is (eq (sheaf:ir-def-name down) (sheaf:ir-call-function (first (calls down)))))))

(test a-cycle-that-builds-only-growables-takes-no-region
  (let* ((main (first (clones "growables" "MAIN-DOWN")))
         (entry (call-to main "growables" "FILLED-DOWN")))
    (is (null (sheaf:ir-call-region entry)))))

(defun banked-prims (def)
  (let ((prims '()))
    (sheaf:walk-ir (lambda (node) (when (and (typep node 'sheaf:ir-prim) (sheaf:ir-prim-bank node)) (push node prims)))
                   (sheaf:ir-def-body def))
    prims))

(test a-cycle-that-builds-a-growable-at-every-level-hands-out-stacks-from-a-bank
  (dolist (down (clones "growables" "VECTORS-DOWN"))
    (is (integerp (sheaf:ir-def-bank down)))
    (is (null (sheaf:ir-def-growables down)))
    (let ((prims (banked-prims down)))
      (is (= 1 (length prims)))
      (is (null (sheaf:ir-prim-stack (first prims))))
      (is (eql (sheaf:ir-def-bank down) (sheaf:ir-prim-bank (first prims)))))
    (is (eql (sheaf:ir-def-bank down) (sheaf:ir-call-bank (first (calls down)))))))

(test a-caller-that-drops-a-banked-result-owns-the-bank
  (let* ((main (first (clones "growables" "MAIN-VECTORS"))) (found nil))
    (sheaf:walk-ir (lambda (node)
                     (when (and (typep node 'sheaf:ir-let) (sheaf:ir-let-bank node))
                       (setf found node)))
                   (sheaf:ir-def-body main))
    (is (null (sheaf:ir-def-bank main)))
    (is (integerp (sheaf:ir-let-bank found)))
    (is (eql (sheaf:ir-let-bank found) (sheaf:ir-call-bank (sheaf:ir-let-value found))))))

(test a-bank-reaches-a-callee-that-builds-in-a-banked-cycle
  (dolist (down (clones "growables" "TWOS-DOWN"))
    (is (integerp (sheaf:ir-def-bank down))))
  (let ((two (find-if #'sheaf:ir-def-bank (clones "growables" "TWO"))))
    (is (null (sheaf:ir-def-growables two)))
    (is (= 2 (length (banked-prims two))))))

(test a-banked-result-frees-its-stacks-with-its-owner
  (is (= 36 (sheaf:run-lowered (assigned "growables")))))

(test size-classes-come-from-the-prelude-types
  (is (eq :resizable (sheaf:size-class *growable*)))
  (is (eq :variable (sheaf:size-class (sheaf::make-ir-type :array :storage 'sheaf/prelude:array (list *number*)))))
  (is (eq :fixed (sheaf:size-class *number*))))

(test a-growable-local-kept-across-a-recursive-call-is-rejected
  (is (search "sheaf/issues/17" (error-text (lambda () (assigned "growable-recursion"))))))

;;; Loops

(defun resets (fixture name)
  "The reset of each call in the first clone of NAME in FIXTURE to a def of its own cycle."
  (let ((def (first (clones fixture name))))
    (loop :for call :in (calls def)
          :when (eq (sheaf:ir-def-source (gethash (sheaf:ir-call-function call) (sheaf:lowered-defs (assigned fixture))))
                    (sheaf:ir-def-source def))
            :collect (sheaf:ir-call-reset call))))

(test a-loop-that-carries-nothing-resets-its-region
  (let ((entry (call-to (first (clones "loops" "RUN")) "loops" "SCRATCH-LOOP")))
    (is (equal (list (sheaf:ir-call-region entry)) (resets "loops" "SCRATCH-LOOP")))))

(test loops-through-two-functions-reset
  (is (every #'integerp (resets "loops" "PING")))
  (is (every #'integerp (resets "loops" "PONG"))))

(test a-loop-that-passes-a-parameter-through-resets
  (is (every #'integerp (resets "loops" "WALK-LIST"))))

(test a-loop-that-carries-what-it-builds-keeps-its-region
  (is (equal '(nil) (resets "tail" "BUILD"))))

(test a-call-that-is-not-a-tail-call-does-not-reset
  (is (equal '(nil) (resets "tail" "TOTAL"))))

(test resetting-loops-keep-their-results
  (is (= 15850 (sheaf:run-lowered (assigned "loops")))))
