(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)
        x2 (+ x x)]

 (Math/pow x2 xx))
)


(defn spiff [v]
  (cond
  (number? (get v 2))
  (let [a (get v 0)
        b (get v 2)]
    (+ a b))
   :else nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (if (> (count v) 2)
   (let [a (get v 0)
        b (get v 2)]
      (+ a b)
      )
     nil
))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)]
        (- d c)
      )
)


  (defn height [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 1)
        d(get b 1)]
    (- d c)
      )
)

(defn square? [rectangle]
   (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)
        e (get a 1)
        f(get b 1)
         cd (* d c)
         ef (* e f)]
      (if (= cd ef)
    true
    (if(= cd 0)
      true
      (if (= ef 0)
      true
      false)
      ))))

(defn area [rectangle]
     (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)
        e (get a 1)
        f(get b 1)
         cd (- c d)
         ef (- e f)]
       (* cd ef)
      ))

"copy-paste from stackoverflow...http://stackoverflow.com/questions/21753243/absolute-value-of-a-number-in-clojure"
(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                             "abs requires a number"))
   (neg? n) (- n)
   :else n))


(defn getNumbers?[x y]
  (if(and (< x 0) (< y 0))
    [(abs x) (abs y)]
    [x y]
))



(defn contains-point? [rectangle point]
(let [a (get  rectangle 0)
        b (get rectangle 1)
       aa (getNumbers? (get a 0)(get a 1))
      bb (getNumbers? (get b 0) (get b 1))
      eka (abs(get point 0))
      toka (abs(get point 1))]
      (if (and (<= eka (aa 0))(<= toka (aa 1)))
        (if(and(and (= eka 0)(= toka 0)))
          false
          true)
    (if(and (<= eka (bb 0))(<= toka (bb 1)))
       (if(and(and (= eka 0)(= toka 0)))
          false
          true)
      false)
    )))


(defn contains-rectangle? [outer inner]
  (if(=(contains-point? outer (get inner 0)) true)
    true
    (contains-point? outer (get inner 1)))
)


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== (author-count book) 1)
    false
    true)
  )


(defn add-author [book new-author]
  (let [orgAuth (book :authors)
        newAuth (conj orgAuth new-author)
        newBook {:title (book :title)
                     :authors newAuth}
        newBook2 {:authors newAuth}]
    (if (> (title-length newBook) 0)
      newBook
      newBook2
    )
))


(defn alive? [author]
  (let[death(contains? author :death-year)
       really(if (= death true)
         false
         true)]
    really))

(defn laske[x]
  (count x))

(defn element-lengths [collection]
  (map laske collection)
  )

(defn secondIndex [collection2]
  (get collection2 1))

(defn second-elements [collection]
  (let [lista (map secondIndex collection)]
    lista)
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (apply <= a-seq)
   true
   (apply >= a-seq))
)

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem))
)


(defn contains-duplicates? [a-seq]
  (let[summa(count (distinct a-seq))
       summa2(count a-seq)]
    (if(= summa summa2)
      false
      true))
  )

(defn old-book->new-book [book]
  (assoc book :authors  (set (concat (book :authors))))
  )


(defn has-author? [book author]
(contains?(book :authors) author)
  )


(defn authors [books]
  (let [author-names
         (fn [book] (book :authors))]
    (set (apply concat (map author-names books)))))


(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))


(defn author->string [author]
  (let [nimi (author :name)
        synt (author :birth-year)
        death (author :death-year)
        merkki (str" - ")
        kaari (str "(")
        kaari2 (str ")")]
    (if (= synt nil)
    (str nimi)
      (str (str nimi " " ) kaari synt merkki death kaari2)))
  )


(defn authors->string [authors]
(let [result(for [x authors]
  (str (author->string x)))
      result2 (interpose ", " result)
      result3 (apply str result2)]
  result3))


(defn book->string [book]
   (let [title (book :title)
        auth (book :authors)
        authhors (authors->string auth)
        by (str ", written by ")]
      (str title by authhors)))

(defn books->string [books]
  (let [koko (count books)
        result(for [x books]
                (str (book->string x)))
                 result2 (interpose ". " result)
      result3 (apply str result2)]
    (if (= koko 0)
      "No books."
       (if (= koko 1)
      (str koko " book. " result3 ".")
          (str koko " books. " result3 ".")))))


(defn books-by-author [author books]
(filter (fn[x] (= (has-author? x author)true)) books))


(defn author-by-name [name authors]
  (let [result(filter (fn[x] (= (:name x) name)) authors)
        result2(if (= (count result) 0)
        nil
        (first result)
        )
        ]
result2)
  )

(defn living-authors [authors]
  (filter (fn[x] (alive? x))authors))

(defn has-a-living-author? [book]
  (let [allAutohors(:authors book)
        living (living-authors allAutohors)
        returnvalue(if (=(count living) 0)
        false
        true)]
    returnvalue))

(defn books-by-living-authors [books]
  (filter (fn[x] (has-a-living-author? x))books))


; %________%
