;; Loads Quicklisp if it's not already loaded
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Automatically loads packages when I run the program
(eval-when (:compile-toplevel :load-toplevel :execute)
  (quicklisp:quickload "cl-ppcre"))

;; This is a method I wrote instead of using it pre-made by the split-sequence package
  (defun split-sequence (delimiter sequence)
    (labels ((split-helper (seq start end result)
               (if end
                   (split-helper seq (+ end 1) (position delimiter seq :start (+ end 1))
                                 (cons (subseq seq start end) result))
                   (nreverse (cons (subseq seq start) result)))))
      (split-helper sequence 0 (position delimiter sequence) '())))


(defun split-string (string delimiter)
  
  (if delimiter
      (split-sequence (char delimiter 0) string) 
      (cl-ppcre:split "\\s+" string))) 


;; A similar approach to structs in C 
(defparameter *line-types*
  '(:function-declaration
    :if-statement
    :while-loop
    :else-statement
    :printf-statement
    :for-loop
    :function-definition
    :variable-declaration-initialization
    :variable-declaration
    :variable-assignment
    :function-call-assignment
    :return-statement
    :unsupported))

  ;; Using Quickload's cl-ppcre qualities, im comparing the line regex structure to the each line that is read
 (defun line-type (line)
  "Determine the type of the given line of C code."
  (format t "Debug: Checking line type for line: ~a~%" line)
  
  (let ((c-types "int|float|double|char|long|short|unsigned|signed|void|struct|union|enum"))
   
    (if (or (not line) (cl-ppcre:scan "^\\s*$" line))
        :unsupported
        (cond
          ;; Function declaration
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*\\(.*\\)\\s*;" c-types) line) :function-declaration)
          
          ;; If statement
          ((cl-ppcre:scan "^\\s*if\\s*\\(([^)]+)\\)" line) :if-statement)

          ;; Else statement
          ((cl-ppcre:scan "^\\s*else" line) :else-statement)

          ;; While loop
          ((cl-ppcre:scan "^\\s*while\\s*\\(([^)]+)\\)" line) :while-loop)

          ;; printf statement
          ((cl-ppcre:scan "^\\s*printf\\s*\\(" line) :printf-statement)

          ;; For loop
          ((cl-ppcre:scan "^\\s*for\\s*\\((.*);(.*);(.*)\\)" line) :for-loop)

          ;; Function definition: match any C type followed by a function definition
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*\\(.*\\)\\s*\\{" c-types) line) :function-definition)

          ;; Variable declaration with initialization, including arithmetic expressions
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*=\\s*.*[+\\-*/].*;" c-types) line)
           :variable-declaration-initialization)

          ;; Simple variable declaration with initialization (without arithmetic)
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*=\\s*[^;]+;" c-types) line)
           :variable-declaration-initialization)

          ;; Variable declaration without initialization
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*;" c-types) line) :variable-declaration)

          ;; Variable assignment
          ((cl-ppcre:scan "^\\s*\\w+\\s*=\\s*[\\d.]+;" line) :variable-assignment)

          ;; Function call assignment
          ((cl-ppcre:scan (format nil "^\\s*(~a)\\s+\\w+\\s*=\\s*\\w+\\s*\\((.*)\\)\\s*;" c-types) line) :function-call-assignment)

          ;; Return statement
          ((cl-ppcre:scan "^\\s*return\\s+(.*);" line) :return-statement)

          ;; Unsupported line type
          (t :unsupported)))))

;; This method receives the line-type from the from the previous function as a parameter and then calls the appropriate function that handles the translation

(defun conversion-foo (line-type)
  "Return the appropriate conversion function for the given line type."
  (format t "Debug: Getting conversion function for line type: ~a~%" line-type)
  (case line-type
  (:function-declaration 'functionPrototype)
  (:if-statement 'ifStatement)
  (:printf-statement 'printfStatement)
  (:for-loop 'forLoop)
  (:while-loop 'whileLoop)
  (:function-definition 'functionDefinition)
  (:variable-declaration-initialization 'varDeclarationInitialized)
  (:variable-declaration 'varDeclaration)
  (:variable-assignment 'variableAssignment)
  (:function-call-assignment 'functionCallAssignment)
  (:return-statement 'returnStatement)
  (t 'unsupportedLine)))

(defun recursive-conversion (lines &optional (declarations '()) (open-blocks '()))
  "Recursively process each line of C code and convert it to Lisp.
   Keeps track of open blocks and adds closing parentheses when encountering '}'."
  (when lines
    (let* ((line (first lines))
           (type (line-type line)))
      (if (cl-ppcre:scan "^\\s*}\\s*$" line)
          ;; Handle closing brace line
          (cons (close-block open-blocks)
                (recursive-conversion (rest lines) declarations (rest open-blocks)))
      
          (let ((conversion-function (conversion-foo type)))
            (if conversion-function
                (let ((conversion (funcall conversion-function line))
                      (new-open-blocks
                       (if (member type '(:if-statement :for-loop :while-loop :function-definition))
                           (cons type open-blocks)
                           open-blocks)))
                  (cons conversion
                        (recursive-conversion (rest lines) declarations new-open-blocks)))

                ;; Unsupported line
                (cons (format nil ";; Unsupported line: ~a~%" line)
                      (recursive-conversion (rest lines) declarations open-blocks))))))))


(defun close-block (open-blocks)
  "Close the last open block by adding a closing parenthesis ')'."
  (if (null open-blocks)
      ";; Error: unmatched '}'"
      (let ((block (pop open-blocks)))
        (cond
          ((eq block :if-statement) ") ;; End if")
          ((eq block :for-loop) ") ;; End for")
          ((eq block :while-loop) ") ;; End while")
          ((eq block :function-definition) ") ;; End function")
          (t ") ;; End block")))))

(defun translate-type (c-type)
  "Translate C type to Lisp type in lowercase."
  (case (intern (string-upcase c-type))
    (INT 'integer)
    (FLOAT 'float)
    (DOUBLE 'double-float)
    (CHAR 'character)
    (VOID 'nil) 
    (T 't))) 

(defun functionPrototype (line)
  "Convert a function prototype to Lisp in lowercase format."
  (when (cl-ppcre:scan "^\\s*(\\w+)\\s+(\\w+)\\s*\\((.*)\\)\\s*;" line)
    (let* ((return-type (cl-ppcre:regex-replace-all "^\\s*(\\w+)\\s+\\w+\\s*\\(.*" line "\\1")) ;; Capture the return type
           (function-name (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+(\\w+)\\s*\\(.*" line "\\1")) ;; Capture the function name
           (params (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+\\w+\\s*\\((.*)\\)\\s*;" line "\\1")) ;; Capture parameter list
           (param-types (if (string= (string-trim '(#\Space) params) "") ;; Handle no parameters
                            nil
                            (mapcar (lambda (param)
                                      ;; Extract and convert the type of each parameter
                                      (translate-type (first (cl-ppcre:split "\\s+" (string-trim '(#\Space) param)))))
                                    (cl-ppcre:split "\\s*,\\s*" params))))) ;; Split parameters by commas
      ;; Translate return type and format the output with lowercase
      (let ((lisp-return-type (translate-type return-type))
            (lisp-function-name (intern (string-downcase function-name))))
        ;; Convert the formatted output to a lowercase string
        (string-downcase
         (format nil "(declaim (ftype (function (~{~a~^ ~}) ~a) ~a))~%"
                 param-types lisp-return-type lisp-function-name))))))

;; Handle printf statements
(defun printfStatement (line)
  "Convert a C printf statement to Lisp format."
  (when (cl-ppcre:scan "^\\s*printf\\s*\\((.*)\\);" line)
  (let* ((printf-contents (second (cl-ppcre:split "[()]" line)))
       (components (split-string printf-contents ",")))
    (let ((format-string (string-trim '(#\Space) (first components)))
      (variables (mapcar (lambda (x) (string-trim '(#\Space) x)) (rest components))))
    (setf format-string (cl-ppcre:regex-replace-all "\\\\n" format-string "~%"))
    (format nil "(format t ~a ~{~a~})~%" format-string variables)))))

(defun functionDefinition (line)
  "Convert a C function definition to Lisp."
  (when (cl-ppcre:scan "^\\s*(\\w+)\\s+(\\w+)\\s*\\((.*)\\)\\s*\\{" line)
    (let* ((return-type (cl-ppcre:regex-replace-all "^\\s*(\\w+)\\s+\\w+\\s*\\(.*" line "\\1")) ;; Extract return type
           (function-name (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+(\\w+)\\s*\\(.*" line "\\1")) ;; Extract function name
           (params (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+\\w+\\s*\\((.*)\\)\\s*\\{" line "\\1")) ;; Extract parameters
           (param-list (if (string= (string-trim '(#\Space) params) "") ;; Handle empty parameter list
                           nil
                           (mapcar (lambda (param)
                                     ;; Extract only the parameter name (ignoring type)
                                     (second (cl-ppcre:split "\\s+" (string-trim '(#\Space) param))))
                                   (cl-ppcre:split "\\s*,\\s*" params))))
           (param-types (if (string= (string-trim '(#\Space) params) "") ;; Handle empty parameter list
                            nil
                            (mapcar (lambda (param)
                                      ;; Extract only the parameter type
                                      (first (cl-ppcre:split "\\s+" (string-trim '(#\Space) param))))
                                    (cl-ppcre:split "\\s*,\\s*" params))))) ;; Split parameters
      ;; Handle the function definition in Lisp style
      (if (string= function-name "main")
          (format nil "(defun main () ~%)")
          (format nil "(defun ~a (~{~a~^ ~})~%  ;; Return type: ~a~%"
                  function-name
                  param-list
                  return-type)))))

(defun varDeclaration (line)
  "Handle variable declaration without initialization for any C type."
  
  (let ((c-types "int|float|double|char|long|short|unsigned|signed|void|struct|union|enum"))
    (when (cl-ppcre:scan (format nil "^\\s*(~a)\\s+(\\w+)\\s*;" c-types) line)
      (let ((var-name (cl-ppcre:regex-replace-all (format nil "^\\s*(~a)\\s+(\\w+)\\s*;" c-types) line "\\2")))  ;; Extract variable name
        (format nil "(defvar ~a)~%" var-name)))))

(defun variableAssignment (line)
 
  (when (cl-ppcre:scan "^\\s*(\\w+)\\s*=\\s*([^;]+)\\s*;" line)
    (let* ((var-name (cl-ppcre:regex-replace-all "^\\s*(\\w+)\\s*=.*" line "\\1")) ;; Extract variable name
           (value (cl-ppcre:regex-replace-all "^\\s*\\w+\\s*=\\s*([^;]+)\\s*;" line "\\1")))  ;; Extract the value
      (format nil "(setf ~a ~a)~%" (string-trim '(#\Space) var-name) (string-trim '(#\Space) value)))))


(defun functionCallAssignment (line)
  (when (cl-ppcre:scan "^\\s*int\\s+(\\w+)\\s*=\\s*(\\w+)\\s*\\((.*)\\);" line)
    (let* ((var-name (cl-ppcre:regex-replace-all "^\\s*int\\s+(\\w+)\\s*=.*" line "\\1"))
           (function-call (cl-ppcre:regex-replace-all "^\\s*int\\s+\\w+\\s*=\\s*(\\w+\\(.*\\));" line "\\1"))
           (function-call-no-commas (cl-ppcre:regex-replace-all "," function-call " ")))
      (format nil "(let* ((~a ~a))~%" var-name function-call-no-commas))))


  (defun returnStatement (line)
    (when (cl-ppcre:scan "^\\s*return\\s+(.*);" line)
      (let* ((return-value (cl-ppcre:regex-replace-all "^\\s*return\\s+" line ""))  
             (return-value (string-trim '(#\Space #\;) return-value))  
             (components (split-string return-value " ")))  
        ;; Debugging: Check what was extracted
        (format t "Debug: Extracted return-value: ~a~%" return-value)
        (format t "Debug: Components extracted: ~a~%" components)

        (if return-value
            ;; Check if there's an operator like +, -, *, /
            (if (some (lambda (comp) (cl-ppcre:scan "\\+|\\-|\\*|\\/" comp)) components)
                ;;  expression with an operator
                (let* ((operator (first (remove-if-not (lambda (word) (cl-ppcre:scan "\\+|\\-|\\*|\\/" word)) components)))
                       
                       (operands (mapcar (lambda (x) (string-trim '(#\Space) x))
                                         (cl-ppcre:split (format nil "\\s*~a\\s*" (cl-ppcre:quote-meta-chars operator)) return-value))))
                s
                  (format t "Debug: Operator: ~a, Operands: ~a~%" operator operands)
                  ;; Reformat the expression into Lisp's prefix notation 
                  (format nil "(return (~a ~{~a~^ ~}))~%"
                          (cond
                            ((string= operator "+") '+)
                            ((string= operator "-") '-)
                            ((string= operator "*") '*)
                            ((string= operator "/") '/))
                          operands))
             
              (cond
                ;; If it's an integer
                ((cl-ppcre:scan "^\\d+$" return-value)
                 (format nil "(return ~a)~%" (parse-integer return-value)))
                ;; If it's a float
                ((cl-ppcre:scan "^\\d+\\.\\d+$" return-value)
                 (format nil "(return ~a)~%" (coerce (read-from-string return-value) 'float)))  ;; Parse float
                ;; If it's a variable or unknown identifier
                (t (format nil "(return ~a)~%" (string-trim '(#\Space) return-value)))))
          
          "(return NIL)~%"))))


   ;; Matches any C type (int, float, double, etc.), variable name, and value 
  (defun varDeclarationInitialized (line)
    "Handle variable declaration with initialization, such as 'double x = 10.0;'."
   
    (when (cl-ppcre:scan "^\\s*(\\w+)\\s+(\\w+)\\s*=\\s*([^;]+)\\s*;" line)
      (let* ((var-type (cl-ppcre:regex-replace-all "^\\s*(\\w+)\\s+.*" line "\\1")) 
             (var-name (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+(\\w+)\\s*=.*" line "\\1")) 
             (value (cl-ppcre:regex-replace-all "^\\s*\\w+\\s+\\w+\\s*=\\s*([^;]+)\\s*;" line "\\1"))) ;; Extract the value (integer, float, or string)
       
        (if (and var-type var-name value)
            
            (format nil "(let ((~a ~a))~%)" var-name value)
         
          (format nil ";; Invalid variable declaration: ~a~%" line)))))

(defun convert-logical-condition (condition)
  "Convert a logical condition from C to Lisp."
  (let ((components (split-string condition " ")))
  (cond
    ((member "&&" components)
     (let ((parts (split-string condition "&&")))
     (format nil "(and ~a ~a)"
         (convert-logical-condition (string-trim '(#\Space) (first parts)))
         (convert-logical-condition (string-trim '(#\Space) (second parts))))))
    ((member "||" components)
     (let ((parts (split-string condition "||")))
     (format nil "(or ~a ~a)"
         (convert-logical-condition (string-trim '(#\Space) (first parts)))
         (convert-logical-condition (string-trim '(#\Space) (second parts))))))
    (t
     (let ((operator (first (remove-if-not (lambda (word) (cl-ppcre:scan "<|>|<=|>=|==|!=" word)) components)))
       (operands (remove-if (lambda (word) (cl-ppcre:scan "<|>|<=|>=|==|!=" word)) components)))
     (format nil "(~a ~a ~a)"
         (cond
           ((string= operator "<") '<)
           ((string= operator ">") '>)
           ((string= operator "<=") '<=)
           ((string= operator ">=") '>=)
           ((string= operator "==") '=)
           ((string= operator "!=") '/=))
         (string-trim '(#\Space) (first operands))
         (string-trim '(#\Space) (second operands))))))))

(defun ifStatement (line)
  "Handle if statements."
  (when (cl-ppcre:scan "^\\s*if\\s*\\(([^)]+)\\)" line)
  (let ((condition (cl-ppcre:regex-replace-all "^\\s*if\\s*\\(([^)]+)\\)" line "\\1")))
    (let ((components (split-string condition " ")))
    (if (some (lambda (comp) (cl-ppcre:scan "\\+|\\-|\\*|\\/" comp)) components)
      (let* ((operator (first (remove-if-not (lambda (word) (cl-ppcre:scan "\\+|\\-|\\*|\\/" word)) components)))
           (operands (remove-if (lambda (word) (cl-ppcre:scan "\\+|\\-|\\*|\\/" word)) components)))
        (format nil "(if (~a ~a ~a)~%"
            (cond
            ((string= operator "+") '+)
            ((string= operator "-") '-)
            ((string= operator "*") '*)
            ((string= operator "/") '/))
            (string-trim " " (first operands))
            (string-trim " " (second operands))))
      (let ((logical-condition (convert-logical-condition condition)))
      (format nil "(if ~a~%" logical-condition)))))))


;; LOOP STRUCTURES
  (defun whileLoop (line)
    "Convert a C while-loop to Lisp format."
   
    (when (cl-ppcre:scan "^\\s*while\\s*\\(([^)]+)\\)" line)
      (let ((condition (cl-ppcre:regex-replace-all "^\\s*while\\s*\\(([^)]+)\\)" line "\\1")))
        
        (let ((logical-condition (convert-logical-condition condition)))
          (format nil "(loop while ~a do~%" logical-condition)))))

  (defun forLoop (line)
    "Convert a C for-loop to Lisp format."
    
    (when (cl-ppcre:scan "^\\s*for\\s*\\(([^;]+);([^;]+);([^\\)]+)\\)" line)
      (let* ((loop-init (cl-ppcre:regex-replace-all "^\\s*for\\s*\\(([^;]+);.*;.*\\)" line "\\1"))  ;; Initialization part
             (loop-condition (cl-ppcre:regex-replace-all "^\\s*for\\s*\\([^;]+;([^;]+);.*\\)" line "\\1"))  ;; Condition part
             (loop-increment (cl-ppcre:regex-replace-all "^\\s*for\\s*\\([^;]+;.*;([^\\)]+)\\)" line "\\1")))  ;; Increment part
      
        (let* ((init (string-trim '(#\Space) loop-init))
               (condition (string-trim '(#\Space) loop-condition))
               (increment (string-trim '(#\Space) loop-increment))
               (var-name (second (split-string init " ")))
               (start-value (third (split-string init " ")))
               (end-value (second (split-string condition " "))))
         
          (format t "Debug: Init: ~a, Condition: ~a, Increment: ~a~%" init condition increment)
         
          (format nil "(loop for ~a from ~a below ~a do~%" 
                  var-name start-value end-value)))))

;; if none of lines match the other 
(defun unsupportedLine (line)
  "Handle unsupported lines by adding them as comments."
  (format nil ";; ~a~%" line))

(defun read-file (input-path)
  "Read the input C file and return its content as a list of lines."
  (with-open-file (in input-path :direction :input)
  (loop for line = (read-line in nil nil)
      while line
      collect line)))

(defun write-file (output-path content)
  "Write the fully converted Lisp code to the output file."
  (with-open-file (out output-path :direction :output :if-exists :supersede :if-does-not-exist :create)
  (dolist (line content)
    (write-string line out))))

(defun convert-c-file (input-path output-path)
  "Convert a C file to Lisp code and write it to a new file."
  (let ((lines (read-file input-path)))
  (write-file output-path (recursive-conversion lines))))

(defun run-conversion ()
  "Main function to call the conversion process."
  (format t "Starting conversion...~%")
  (convert-c-file "sample.c" "output.lisp")
  (format t "Conversion complete!~%"))

(run-conversion)  

