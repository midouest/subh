(local list (includefnl :lib/list))
(local volt (includefnl :lib/volt))

(fn new-rhythm [id num-seqs]
  "Generate the initial state for a single rhythm generator"
  {: id :div 1 :trigs (list.repeat false num-seqs)})

(fn new-rhythms [count num-seqs]
  "Generate the initial set of rhythm generator states"
  (list.repeat-with #(new-rhythm $1 num-seqs) count))

(fn new-seq [id num-steps num-assigns]
  "Generate the initial state for a sequencer"
  {: id
   :step 1
   :steps (list.repeat 0 num-steps)
   :assigns (list.repeat false num-assigns)})

(fn new-seqs [count num-steps num-assigns]
  "Generate the initial set of sequencer states"
  (list.repeat-with #(new-seq $1 num-steps num-assigns) count))

(fn new-sub [id]
  "Generate the initial state for a sub-oscillator"
  {: id :div 1 :mix 0})

(fn new-subs [count]
  "Generate the initial set of sub-oscillator states"
  (list.repeat-with #(new-sub $1) count))

(fn new-vco [id num-subs]
  "Generate the initial state for an oscillator"
  {: id :freq 0 :subs (new-subs num-subs) :wave :sqr :mix 0})

(fn new-vcos [count num-subs]
  "Generate the initial set of oscillator states"
  (list.repeat-with #(new-vco $1 num-subs) count))

(fn new [num-rhythms num-steps num-vcos num-subs]
  "Generate the initial subharmonicon state"
  (let [num-seqs num-vcos
        num-assigns (+ 1 num-subs)]
    {: num-rhythms
     : num-steps
     : num-vcos
     : num-subs
     : num-seqs
     : num-assigns
     :beat 0
     :rhythms (new-rhythms num-rhythms num-seqs)
     :rhythm-op :or
     :seqs (new-seqs num-seqs num-steps num-assigns)
     :seq-oct 1
     :vcos (new-vcos num-vcos num-subs)
     :vco-quant :et12
     :vcf-cutoff 0
     :vcf-res 0
     :vcf-eg-amt 0
     :vcf-atk 0
     :vcf-dec 0
     :vca-gain 0
     :vca-atk 0
     :vca-dec 0}))

(fn set-rhythm-div [state r div]
  (tset state :rhythms r :div div))

(fn set-rhythm-trig [state r t trig?]
  (tset state :rhythms r :trigs t trig?))

(fn set-rhythm-op [state op]
  (set state.rhythm-op op))

(fn set-seq-step [state s i offset]
  (tset state :seqs s :steps i offset))

(fn set-seq-assign [state s a assign?]
  (tset state :seqs s :assigns a assign?))

(fn set-seq-oct [state seq-oct]
  (set state.seq-oct seq-oct))

(fn set-vco-freq [state v freq]
  (tset state :vcos v :freq freq))

(fn set-vco-mix [state v mix]
  (tset state :vcos v :mix mix))

(fn set-vco-wave [state v wave]
  (tset state :vcos v :wave wave))

(fn set-sub-div [state v s div]
  (tset state :vcos v :subs s :div div))

(fn set-sub-mix [state v s mix]
  (tset state :vcos v :subs s :mix mix))

(fn set-vco-quant [state vco-quant]
  (set state.vco-quant vco-quant))

(fn set-vcf-cutoff [state vcf-cutoff]
  (set state.vcf-cutoff vcf-cutoff))

(fn set-vcf-res [state vcf-res]
  (set state.vcf-res vcf-res))

(fn set-vcf-eg [state vcf-eg]
  (set state.vcf-eg vcf-eg))

(fn set-vcf-atk [state vcf-atk]
  (set state.vcf-atk vcf-atk))

(fn set-vcf-dec [state vcf-dec]
  (set state.vcf-dec vcf-dec))

(fn set-vca-gain [state vca-gain]
  (set state.vca-gain vca-gain))

(fn set-vca-atk [state vca-atk]
  (set state.vca-atk vca-atk))

(fn set-vca-dec [state vca-dec]
  (set state.vca-dec vca-dec))

(fn new-moog []
  "Initialize a Subharmonicon with the original Moog configuration:
- 4 rhythm generators
- 4 sequencer steps
- 2 VCOs
- 2 sub-oscillators per VCO"
  (new 4 4 2 2))

(fn inc-beat [state]
  "Increment the primary clock beat"
  (set state.beat (+ 1 state.beat)))

(fn divides-beat? [state rhythm]
  "Returns true if the given rhythm subdivides the current beat"
  (= (% state.beat rhythm.div) 0))

(fn count-trigs [state]
  "Count the number of times each sequencer is triggered by any rhythm"
  (let [counts (list.repeat 0 state.num-seqs)]
    (each [_ rhythm (ipairs state.rhythms)]
      (when (divides-beat? state rhythm)
        (each [i trig? (ipairs rhythm.trigs)]
          (when trig?
            (tset counts i (+ 1 (. counts i)))))))
    counts))

(fn apply-rhythms [state]
  "Returns true for each sequencer if that sequencer has been triggered on the
current beat, otherwise false."
  (let [counts (count-trigs state)
        trigger (match state.rhythm-op
                  :or #(> $1 0)
                  :xor #(= $1 1))]
    (icollect [_ count (ipairs counts)]
      (trigger count))))

(fn get-vco [state i]
  "Look up the VCO with the given ID"
  (. state.vcos i))

(fn get-seq [state i]
  "Look up the sequencer with the given ID"
  (. state.seqs i))

(fn get-step-offset [seq]
  "Get the sequencer step offset for the current sequencer step index"
  (. seq.steps seq.step))

(fn inc-step [state step]
  "Increment a step and wrap back to step 1"
  (+ 1 (% step state.num-steps)))

(fn advance-seq [state i]
  "Advance the sequencer step index"
  (let [seq (get-seq state i)]
    (set seq.step (inc-step state seq.step))))

(fn reset-seqs [state]
  "Reset all sequencers to their first step"
  (each [_ seq (ipairs state.seqs)]
    (set seq.step 1)))

(fn reset [state]
  "Reset the beat and all sequencers"
  (set state.beat 0)
  (reset-seqs state))

(fn vco-assigned? [seq]
  "True if the given sequencer affects the associated VCO"
  (. seq.assigns 1))

(fn sub-assigned? [seq i]
  "True if the given sequencer affects the associated sub-oscillator ID"
  (->> i
       (+ 1)
       (. seq.assigns)))

(fn new-trig-effect []
  {:type :trig})

(fn new-vco-effect [id freq]
  {:type :vco : id : freq})

(fn new-sub-effect [id vco-id freq]
  {:type :sub : id : vco-id : freq})

(fn get-vco-effects [state i only-changed?]
  "Compute VCO and sub-oscillator effects for the current sequencer state. If
the last argument is true, then only return effects for components that were
affected by the sequencer."
  (let [effects []
        seq (get-seq state i)
        vco (get-vco state i)
        seq-oct state.seq-oct
        step-offset (get-step-offset seq)
        freq-offset (util.linlin volt.min volt.max (- seq-oct) seq-oct
                                 step-offset)
        seq-vco-change? (vco-assigned? seq)
        seq-vco-offset (if seq-vco-change? freq-offset 0)
        vco-freq (volt.clamp-v (+ vco.freq seq-vco-offset))]
    (if (or (not only-changed?) seq-vco-change?)
        (table.insert effects (new-vco-effect vco.id vco-freq)))
    (icollect [i sub (ipairs vco.subs) :into effects]
      (let [seq-sub-change? (sub-assigned? seq i)]
        (when (or (not only-changed?) seq-vco-change? seq-sub-change?)
          (let [div-offset (if seq-sub-change? (+ sub.div step-offset) sub.div)
                divisor (util.linlin volt.min volt.max 16 1 div-offset)
                sub-freq (volt.divide-v vco-freq divisor)]
            (new-sub-effect sub.id vco.id sub-freq)))))))

(fn trigger-seq [state i]
  "Trigger the sequencer at the given index, returning VCO side-effects"
  (advance-seq state i)
  (get-vco-effects state i true))

(fn tick [state]
  "Advance the clock and resolve sequencer and oscillator state changes"
  (var any-trig? false)
  (let [effects []
        trigs (apply-rhythms state)]
    (each [i trig? (ipairs trigs)]
      (when trig?
        (set any-trig? true)
        (->> i
             (trigger-seq state)
             (list.extend effects))))
    (when any-trig?
      (table.insert effects (new-trig-effect)))
    (inc-beat state)
    effects))

{: new
 : new-moog
 : reset
 : tick
 : set-rhythm-div
 : set-rhythm-trig
 : set-rhythm-op
 : set-seq-step
 : set-seq-assign
 : set-seq-oct
 : set-vco-freq
 : set-vco-mix
 : set-sub-div
 : set-sub-mix
 : set-vco-quant
 : set-vcf-cutoff
 : set-vcf-res
 : set-vcf-eg
 : set-vcf-atk
 : set-vcf-dec
 : set-vca-gain
 : set-vca-atk
 : set-vca-dec}

