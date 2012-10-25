(ns structured-data)

; Harjoitus 1
(defn do-a-thing
	[x]
 	(let [tuplax (+ x x)]
		(Math/pow tuplax tuplax)))

; Harjoitus 2
(defn spiff
        [v]
        (+ (get v 0) (get v 2)))

; Harjoitus 3
(defn cutify
	[v]
	(conj v "<3"))

; Harjoitus 4
(defn spiff-destructuring
	[v]
	(let [[x y z] v]
		(str x y z)
		(+ x z)))

; Valmiina
(defn point [x y]
  [x y])

; Valmiina
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

; Harjoitus 5a
(defn width
        [rectangle]
        (let [[[x1 y1] [x2 y2]] rectangle]
                (str x1 y1 x2 y2)
                (- x2 x1)))

; Harjoitus 5b
(defn height
	[rectangle]
        (let [[[x1 y1] [x2 y2]] rectangle]
                (str x1 y1 x2 y2)
                (- y2 y1)))

; Harjoitus 6
(defn square?
	[rectangle]
	(let [[[x1 y1] [x2 y2]] rectangle]
                (str x1 y1 x2 y2)
		(if(== (- x2 x1) (- y2 y1))
			true
			false)))

; Harjoitus 7
(defn area
	[rectangle]
        (let [[[x1 y1] [x2 y2]] rectangle]
                (str x1 y1 x2 y2)
		(* (- x2 x1) (- y2 y1))))

; Harjoitus 8
(defn contains-point?
	[rectangle point]
        (let [[[x1 y1] [x2 y2]] rectangle
		[x3 y3] point]
                (str x1 y1 x2 y2)
		(str x3 y3)
		(if (and (<= x1 x3 x2) (<= y1 y3 y2))
			true
			false)))

; Harjoitus 9
(defn contains-rectangle?
	[outer inner]
	(let [[[xi1 yi1] [xi2 yi2]] inner]
		(str xi1 yi1 xi2 yi2)

		(let [piste1 [xi1 yi1]
			piste2 [xi2 yi2]]
(if (and (contains-point? outer piste1) (contains-point? outer piste2))
	true
	false))))

; Harjoitus 10
(defn title-length
	[book]
	(count (:title book)))

; Harjoitus 11
(defn author-count
	[book]
	(count (:authors book)))

; Harjoitus 12
(defn multiple-authors?
	[book]
	(if (> (author-count book) 1)
		true
		false))

; Harjoitus 13
; Kaytettava assoc ja conj
(defn add-author
	[book new-author]
	(assoc book :authors
		(conj (:authors book) new-author)
	)
)

; Harjoitus 14
(defn alive?
	[author]
	(if (contains? author :death-year)
		false
		true))

; Harjoitus 15
(defn element-lengths
	[collection]
	(map count collection)
)	

; Harjoitus 16
(defn second-elements
	[collection]
	(let [munge (fn [collection] (get collection 1))]
	(map munge collection))
)

; Harjoitus 17
(defn titles
	[books]
	(map :title books)
)

; Harjoitus 19
; Kaytettava apply
(defn monotonic?
	[a-seq]
	(if (or (apply <= a-seq) (apply >= a-seq))
		true
		false)
)

; Harjoitus 18
(defn stars
	[n]
	(apply str (repeat n "*"))
)

; Harjoitus 20
(defn toggle
	[a-set elem]
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)
	)
)

; Harjoitus 21
(defn contains-duplicates?
	[a-seq]
	(if (== (count a-seq) (count (set a-seq)))
		false
		true
	)
)

; Harjoitus 22
(defn old-book->new-book
	[book]
	(let [setiksi (set (:authors book))]
		(assoc book :authors setiksi)
	)
)

; Harjoitus 23
(defn has-author?
	[book author]
	(if (contains? (:authors book) author)
		true
		false
	)
)

; Harjoitus 24
(defn authors
	[books]
	(apply clojure.set/union (map :authors books))
)

; Harjoitus 25
; Kaytettava tehtavan 24 funktiota authors
; Palautettava set
(defn all-author-names
	[books]
	(set (map :name (authors books)))
)

; Harjoitus 26
; Vaikea
(defn author->string
	[author]
	(let [nimi (:name author)
		svuosi (:birth-year author)
		kvuosi (:death-year author)
		]
		(if (contains? author :death-year)
			(str nimi " ("svuosi " - " kvuosi ")")
			(if (contains? author :birth-year)
				(str nimi " ("svuosi " - )")
				nimi
			)
		)
	)
)

; Harjoitus 27
(defn authors->string
	[authors]
	(apply str (interpose ", " (map author->string authors)))
)

; Harjoitus 28
(defn book->string
	[book]
	(str (:title book) ", written by "
		(authors->string (:authors book))
	)
)

; Harjoitus 29
; Huh
(defn books->string
	[books]
	(cond
	(== (count books) 0)
		"No books."
	(== (count books) 1)
		(str "1 book. " (apply str (map book->string books)) ".")
	(>= (count books) 2)
		(str (count books) " books. "
		(apply str (interpose ". " (map book->string books))) "."
		)
	)
)

; Harjoitus 30
(defn books-by-author
	[author books]
	(filter (fn [dummy] (has-author? dummy author)) books)
)

; Harjoitus 31
(defn author-by-name
	[name authors]
	(let [ vertailu (fn [dummy] (= (:name dummy) name))]
		(first (filter vertailu authors))
	)
)

; Harjoitus 32
; alive?!!!
(defn living-authors
	[authors]
	(filter alive? authors)
)

; Harjoitus 33
(defn has-a-living-author?
	[book]
	(not (empty? (living-authors (:authors book))))
)

; Harjoitus 34
(defn books-by-living-authors
	[books]
	(filter has-a-living-author? books)
)
