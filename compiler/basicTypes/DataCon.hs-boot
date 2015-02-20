module DataCon where
import Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import FieldLabel ( FieldLabel )
import Unique ( Uniquable )
import Outputable ( Outputable, OutputableBndr )

data DataCon
data DataConRep
dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConFieldLabels :: DataCon -> [FieldLabel]
isVanillaDataCon :: DataCon -> Bool

instance Eq DataCon
instance Ord DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
