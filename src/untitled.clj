(use 'structured-data)

(spiff-destructuring [1 2 3])       ;=> 4
(spiff-destructuring [1 2 3 4 5 6]) ;=> 4
(spiff-destructuring [1 2])         ;=> ?
(spiff-destructuring [])            ;=> ?

(cutify []) ;=> ["<3"]
(cutify [1 2 3]) ;=> [1 2 3 "<3"]
(cutify ["a" "b"]) ;=> ["a" "b" "<3"]

(height (rectangle [1 1] [5 1])) ;=> 0
(height (rectangle [1 1] [5 5])) ;=> 4
(height (rectangle [0 0] [2 3])) ;=> 3

(width (rectangle [1 1] [5 1]))  ;=> 4
(width (rectangle [1 1] [1 1]))  ;=> 0
(width (rectangle [3 1] [10 4])) ;=> 7


(square? (rectangle [1 1] [2 2])) ;=> true
(square? (rectangle [1 1] [2 3])) ;=> false
(square? (rectangle [1 1] [1 1])) ;=> true
(square? (rectangle [3 2] [1 0])) ;=> true
(square? (rectangle [3 2] [1 1])) ;=> false

(area (rectangle [1 1] [5 1])) ; => 0
(area (rectangle [0 0] [1 1]));  => 1
(area (rectangle [0 0] [4 3]));  => 12
(area (rectangle [3 1] [10 4]));=> 21

(contains-point? (rectangle [0 0] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point 2 1))            ;=> true
(contains-point? (rectangle [0 0] [2 2])
                 (point -3 1))           ;=> false
(contains-point? (rectangle [0 0] [2 2])
                 (point 1 3))            ;=> false
(contains-point? (rectangle [1 1] [2 2])
                 (point 1 1))            ;=> true
(contains-point? (rectangle [1 1] [1 1])
                 (point 1 1))            ;=> true

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2])) ;=> true
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3])) ;=> false
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [0 0] [1 1])) ;=> true
(contains-rectangle? (rectangle [0 0] [1 1])
                     (rectangle [1 1] [2 2])) ;=> false

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(title-length cities)         ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18
