(require '(clojure.java [io :as io]))
(require '(clojure [string :as str]))

(defn read-file [name]
  (with-open [reader (io/reader name)]
    (doall (line-seq reader))))

(defn tokenize [line]
  (let [line   (str/trim line)
        number (re-matches #"^(\d+)(.*)" line)]
    (cond
      (= (count line) 0) ()
      number (let [[_ value rest] number]
               (cons (Integer/parseInt value) (tokenize rest)))
      :else (let [value (subs line 0 1)
                  rest  (subs line 1)]
              (cons value (tokenize rest))))))

(defn operator? [token]
  (or (= token "+") (= token "*")))
(defn rpn-expr
  [stack [token & rest] output]
  (cond
    (nil? token) (concat (reverse output) stack)
    (number? token) (rpn-expr stack rest (cons token output))
    (operator? token) ; everything but left-paren is lower precedence, so pop it
      (let [[operators stack] (split-with (partial not= "(") stack)]
        (rpn-expr (cons token stack) rest (concat (reverse operators) output)))
    (= "(" token) (rpn-expr (cons token stack) rest output)
    (= ")" token) ; pop everything off the stack until "("
      (let [[operators [_ & stack]] (split-with (partial not= "(") stack)]
        (rpn-expr stack rest (concat (reverse operators) output)))))

(declare eval-expr)
(defn eval-operator
  [operator [second first & stack] tokens]
  (let [result (operator first second)
        stack (cons result stack)]
    (eval-expr stack tokens)))
(defn eval-expr
  [stack [token & rest]]
  (cond
    (nil? token) (first stack)
    (number? token) (eval-expr (cons token stack) rest)
    (= "+" token) (eval-operator + stack rest)
    (= "*" token) (eval-operator * stack rest)))

(defn parse [line]
  (let [tokens (tokenize line)
        expr (rpn-expr () tokens ())
        result (eval-expr () expr)]
    result))

(println (reduce + (map parse (read-file "input"))))
