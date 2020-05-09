(ns wuhanclan.core
    (:use amoebas.defs amoebas.lib amoebas.run amoebas.examples) 
   )




(defn create-wuhaner
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [

                do-hit2  (fn []
                (let
                    [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                         ;; eat
                     {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                  )
            )
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    hs  (hostiles species Neighbors env)
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]
                                
                                (if (empty? empty-nb)  
                                    (if (empty? hs)     ;; no empty neighbors?
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    (do-hit2)
                                )

                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                
                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            {:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...
                        )
            ]

            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)

;;(defn most-energy-target-selector
  ;;  "picks a target with the highest amount of energy stored"
    ;;[hs species env]
    ;;
  ;;  (last (sort-by #(:energy (:occupant (env %))) hs)) 
;;)
     


(def Evam (create-wuhaner 10 30 most-energy-and-fuel-target-selector))
;;
;;  A very simple example of random mutation. With some probability, this species modifies its
;;  own parameters when it divides.
;;