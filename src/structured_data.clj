(ns structured-data)

(defn do-a-thing [x]
   (let [x2 (+ x x)]
  (Math/pow x2 x2)))

(defn spiff [v]
    (let [firstEl (first v)
        thirdEl (get v 2)]
    (+ firstEl thirdEl)))

(defn cutify [v]
    (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn distanceBWPoints [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (int (distanceBWPoints [x2 y2] [x1 y2]))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (int (distanceBWPoints [x2 y2] [x2 y1]))))

(defn square? [rectangle]
  (let [height (height rectangle)
        width (width rectangle)]
    (if (= height width)
      true false)))

(defn area [rectangle]
  (let [height (height rectangle)
        width (width rectangle)]
    (int (* height width))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (or (<= x1 p1 x2) (<= x2 p1 x1))
             (or (<= y1 p2 y2) (<= y2 p2 y1)))
      true false
      )
    ))

(defn contains-rectangle? [outer inner]
    (let [[p1 p2] inner]
    (if (and (contains-point? outer p1) (contains-point? outer p2))
      true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true false))

(defn add-author [book new-author]
  (let [author (:authors book)
        newAuthorList (conj author new-author)]
    (assoc book :authors newAuthorList)
    ))

(defn alive? [author]
  (if (not(:death-year author))
    true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
    (let [ fetchSecondElement (fn [vector] (second vector))]
    (seq (map fetchSecondElement collection))))

(defn titles [books]
    (let [fetchBookTitles (fn [book] (:title  book))]
    (map fetchBookTitles books)))

(defn monotonic? [a-seq]
  (let [monotonic (fn [x comparator] (apply comparator x))]
    (if (or (monotonic a-seq <=) (monotonic a-seq >=))
      true false)
    ))

(defn stars [n]
  (let [repeater (fn [n] (repeat n "*"))]
    (apply str (repeater n))))

(defn toggle [a-set elem]
  ( let [removeItem (fn [oldSet item] (disj oldSet item))
         addItem (fn [oldSet item] (conj oldSet item))]
    (if (contains? a-set elem)
      (removeItem a-set elem) (addItem a-set elem))))

(defn returnSet [x] (set x))

(defn returnCount [x] (count x))

(defn contains-duplicates? [a-seq]
    (if (= (returnCount a-seq) (returnCount (returnSet a-seq)))
      false true))


(defn old-book->new-book [book]
    (assoc book :authors (returnSet (:authors book))))

(defn has-author? [book author]
    (cond
    (and (contains? (:authors book) author)) true
    :else false))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
    ( let
    [authorName (:name author)
     year (cond
          (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
          (contains? author :birth-year) (str " (" (:birth-year author) " - " ")")
          :else "")
     ]
    (str authorName year)
    ))

(defn authors->string [authors]
(  let [separator ", "]
    (apply str (interpose separator (map author->string authors)))))

(defn book->string [book]
  (let [fillerText "written by "
        bookTitle (:title book)
        authorNames (authors->string (:authors book))
        separator ", "]
    (str bookTitle separator fillerText authorNames)
    ))

(defn books->string [books]
    (let [setOfBooks (set books)
        countOfBooks (count setOfBooks)
        returnBookName&Author (fn [x] (apply str (interpose ". "(map book->string books))))
        noBookStr "No books."
        oneBookStr " book. "
        multipleBookStr " books. "
        ]
        (cond
                  (= countOfBooks 0) noBookStr
                  (= countOfBooks 1) (str countOfBooks oneBookStr (returnBookName&Author books) ".")
                  :else (str countOfBooks multipleBookStr (returnBookName&Author books) "."))

    ))

(defn books-by-author [author books]
      (let [filterFunction (fn [x] (has-author? x author))]
    (filter filterFunction books)))

(defn author-by-name [name authors]
    (let [filterFunction  (fn [x] (= (:name x) name))]
    (first (filter filterFunction authors))))

(defn living-authors [authors]
  (let [filterFunction (fn [x] (alive? x))]
  (filter filterFunction authors)))

(defn has-a-living-author? [book]
  (if (not (empty? (living-authors (:authors book))))
    true false))

(defn books-by-living-authors [books]
  (let [filterFunction (fn [x] (has-a-living-author? x))]
    (filter filterFunction books)
    ))

; %________%
