# The representation selection algorithm

Assumes:
 - A property table for each representation `RS1, ..., RSN`
 - A property table for the question `Q` in the current representation, `RS1`
 - A table of correspondences

Given:
 - A question, `Q`, in a representation, `RS1`
 - An integer, `k`, denoting a desired number of alternative representations
    * A special case when `k=-1`: return all viable alternatives

Returns:
 - A list of length `k` containing alternative representations for `Q`
    * This list is sorted (high to low) based on representation relevance

Setup:
1. Load the required property tables and correspondence table
2. Build a function `relevanceScore` to compute RS relevance against Q (see "Relevance")

Procedure:
1. for each RS calculate the relevance score against `Q` with `relevanceScore`
2. discard any RSs with relevance score <= 0
3. sort the RSs using this score, high to low
4. if there are fewer than `k` RSs, or k=-1, return all the RSs
5. otherwise return the first `k` RSs

Relevance:
 - Relevance is defined as the composition of three things:
    * property influence
    * user influence
    * task influence
 - For now, we ignore the last two and instead only consider property influence.
 - Property influence is mostly a result of *correspondence*, i.e. an expert-identified feature of problem and representation properties that result in likely similarities or "matchings"
 - The procedure for calculating the property influence is as follows:
   1. Fix `Q` and `RS` as the question and potential representation, respectively.
   2. A correspondence found in the correspondence table can be thought of as `(left, right, val)`:
    * `left` denotes conditions on the question properties, which we have extended to allow logical statements. For example, we can require `Q` have properties `A` or `B`, but not `C` as ![(A \/ B) /\ -C](https://latex.codecogs.com/gif.latex?%28A%20%5Clor%20B%29%20%5Cland%20%5Clnot%20C).
    * `right` denotes conditions on the representation properties, using the same notation as `left`.
    * `val` is a real number where the sign denotes whether the relationship is good or bad, while the magnitude its strength.
   3. For each correspondence in the correspondence table, check if `left` is matched by `Q` and `right` by `RS`. We leave the meaning of "match" abstract for now. It is obvious for some properties (e.g., `associative` matches `associative`) but others may need more complex definitions of match (e.g., `3-dimensions` matches `atleast-2-dimensions`).
   4. Combine the `val`s of every matched correspondence. For now, *combine* means *sum*.
