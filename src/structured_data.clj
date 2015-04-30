(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
      (Math/pow twox twox)
    )
)

(defn spiff [v]
  (+ (get v 0) (get v 2) )
  
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
    (let [[x y z] v]
    (+ x z)
    )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle
        dx (- x1 x2)]
        (if (< dx 0)
            (- dx)
            dx
        )
    )
)

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle
        dy (- y1 y2)]
        (if (< dy 0)
            (- dy)
            dy
        )
    )
)

(defn square? [rectangle]
    (if (= (height rectangle) (width rectangle))
        true
        false
    )
)

(defn area [rectangle]
    (* (height rectangle) (width rectangle))
)

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
      
      (cond
            (and (<= x1 x2) (<= y1 y2) (>= px x1) (<= px x2) (>= py y1) (<= py y2)) true
            (and (< x1 x2) (> y1 y2) (>= px x1) (<= px x2) (<= py y1) (>= py y2)) true
            
            (and (>= x1 x2) (<= y1 y2) (<= px x1) (>= px x2) (>= py y1) (<= py y2)) true
            (and (> x1 x2) (> y1 y2) (<= px x1) (>= px x2) (<= py y1) (>= py y2)) true
            
            :else         false)
    )
)

(defn contains-rectangle? [outer inner]
    (let [ [ixy1 ixy2] inner]
      (and (contains-point? outer ixy1) (contains-point? outer ixy2))
    )
)

(defn title-length [book]
    (count (get book :title))
)

(defn author-count [book]
    (count (get book :authors))
)

(defn multiple-authors? [book]
    (if (> (count (get book :authors)) 1)
      true
      false)
)

(defn add-author [book new-author]
    (let [original (get book :authors)]
      (assoc book :authors (conj original new-author))
    )
    
)

(defn alive? [author]
    (not (contains? author :death-year))
)

(defn element-lengths [collection]
    (map count collection)
)

(defn second-elements [collection]
    (let [getsecond (fn [x] (get x 1))]
    (map getsecond collection))
)

(defn titles [books]
    (map :title books)
)

(defn monotonic? [a-seq]
    (<= a-seq)
)

(defn stars [n]
    (apply str (repeat n "*"))
)

(defn toggle [aset elem]
    (let [kivonva (count (disj aset elem))
        orig (count aset)]
    
        (if (= orig kivonva)
            (conj aset elem)
            (disj aset elem)
        )
    )
)

(defn contains-duplicates? [a-seq]
    (not (apply distinct? a-seq))
)

(defn old-book->new-book [book]
    (assoc book :authors (set (get book :authors)) )
)

(defn has-author? [book author]
    (if (get (get book :authors) author)
    true
    false)
)

(defn authors [books]
    (set (apply concat (map :authors books)))
)

(defn all-author-names [books]
    (set (map :name (authors books)))
)

(defn author->string [author]
    (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)]
    
    (if (nil? name)
        ""
        (if (not (nil? birth))
            (str name " (" birth " - " death ")" )
            (str name)
        )
    )
    )
)

(defn authors->string [authors]
        (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
    (let [title (get book :title)
        writer (authors->string (set (get book :authors)) ) ]
    
    (str title ", written by " (apply str writer) )
    )
)

(defn books->string [books]
    (let [db (count books)
        writeout (map book->string books)]
    
    (if (= db 0)
        "No books."
        (if (= db 1)
            (apply str "1 book. " (apply str writeout) ".")
            (apply str db " books. " (apply str (interpose ". " writeout)) ".")
        )
    )
    )
)


(defn books-by-author [author books]
    :-
)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
