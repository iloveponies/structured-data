(ns structured-data)

(defn do-a-thing [x]
 (let [xx (+ x x)]                                    ;=> Name operation x + x as xx
  (Math/pow xx xx)))

(defn spiff [v]                                       ;=> Return value of sum of first
  (+ (get v 0) (get v 2)))                            ;=> and third index of given vector

(defn cutify [v]                                      ;=> takes vector and adds
  (conj v "<3"))                                      ;=> "<3" to its end

(defn spiff-destructuring [v]
  (let [[x y z] v]                                    ;=> Return value of sum of first
  (+ x z)))                                           ;=> and third index of given vector

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn point [x y]
  [x y])

(defn getx [point]
  (get point 0))                                      ;=> Return x coordinate of point

(defn gety [point]
  (get point 1))                                      ;=> Return y coordinate of point

(defn smaller? [a b]                                  ;=> Compare x, y of point a and point b
  (if (and (<= (getx a) (getx b))                     ;=> Return true if x, y of point a smaller
           (<= (gety a) (gety b))) true false))       ;=> Else return false

(defn width [rectangle]
  (let [[bl tr] rectangle]                            ;=> Name bl bottom-left, tr top-right
    (- (getx tr) (getx bl))))                         ;=> Return subtraction of tr[0], bl[0]

(defn height [rectangle]
  (let [[bl tr] rectangle]                            ;=> Name bl bottom-left, tr top-right
    (- (gety tr) (gety bl))))                         ;=> Return subtraction of tr[1], bl[1]

(defn square? [rectangle]                             ;=> Return true if width and height
  (if (= (width rectangle)                            ;=> of rectangle of equal value
         (height rectangle))                          ;=> Else return false
    true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))           ;=> Return width multiplied by height

(defn contains-point? [rectangle point]
  (let [[bl tr] rectangle]                            ;=> Name bl bottom-left, tr top-right
    (if (and (smaller? point tr)                      ;=> Return true if bl smaller and tr bigger
             (smaller? bl point))                     ;=> than point. Else return false
      true false)))

(defn contains-rectangle? [outer inner]
  (let[[ibl itr] inner]                               ;=> Name ibl inner bottom-left, itr top-right
    (if (and (contains-point? outer ibl)              ;=> Return true if both contained by outer
             (contains-point? outer itr))             ;=> Else return false
      true false)))

(defn title-length [book]
  (count (get book :title)))                          ;=> Returns length of :title in map book

(defn author-count [book]
  (count (get book :authors)))                        ;=> Return size of authors in map book

(defn multiple-authors? [book]
  (if (< 1 (author-count book))                       ;=> Return true if size of authors more than 1
    true false))                                      ;=> Else return false

  (defn add-author [book new-author]
    (let [new (assoc book :authors                    ;=> Return new book with new-author
             (conj (get book :authors)                ;=> appended to :authors
                 new-author))]
      new))

(defn alive? [author]                                 ;=> Return false if author has :death-year
  (if (get author :death-year) false true))           ;=> Else return true

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [vectors-in-list
        (fn [x] (get x 1))]                           ;=> Return x[1] of vector (vectors-in-list)
    (map vectors-in-list collection)))                ;=> Return all vectors-in-list of collection

(defn titles [books]
  (map :title books))                                 ;=> Return all :title of collection of books

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq)                            ;=> Return true if increasing sequence
          (apply >= a-seq))                           ;=> or decreasing sequence
    true false))                                      ;=> Else return false

(defn stars [n]
  (apply str (repeat n "*")))                         ;=> Concatenates character "*" times n

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)                                 ;=> Remove elem from a-set if contains? true
    (conj a-set elem)))                               ;=> Else add elem to a-set

(defn contains-duplicates? [a-seq]
  (if(< (count (set a-seq))                           ;=> Return true if count set of a-seq smaller
        (count a-seq)) true false))                   ;=> than count of a-seq. Else return false

(defn old-book->new-book [book]
  ())

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; wot is it
