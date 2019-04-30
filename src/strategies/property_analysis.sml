import "strategies.property_tables";
import "strategies.property_correspondence";

(* meant for processing property tables (e.g., counting number of types,
applying correspondences, collecting attributes, etc.)*)
signature PROPERTY_ANALYSIS =
sig
  structure S : SET;
  val collectTokens : S.t S.set -> S.t S.set;
  val collectTypes : S.t S.set -> S.t S.set;
  val collectOfImportance : S.t S.set -> Importance.importance -> S.t S.set;
end;

structure PropertyAnalysis : PROPERTY_ANALYSIS =
struct
  structure S = PropertySet;


end;
