with Group_Descriptors;
use  Group_Descriptors;

with Ada.Containers.Ordered_Maps;

--
-- As explained in the spec file, this package exports functions
-- which map a pair (sampling, index) into the corresponding
-- sampling-dimensional reduction vector.
--
-- For theoretical reasons (which are not recalled here) it is
-- convenient to use the following algorithm
--
--   1. Choose a finite Coxeter group G of sampling-dimensional
--      unitary transformations
--
--   2. Create a subset H of G by including in H one and only one
--      representative of {h, -h}, where h \in G (the choice can
--      be done in an arbitrary way)
--
--   3. Choose a vector Base such that h*Base /= Base for every
--      h \in H
--
--   4. Enumerate all the elements in H. Let H(i) the i-th element
--      of H
--
--   5. Associate to index Idx reduction vector
--      Transpose(Base)*H(idx) (is more convenient to work with
--      the transposed version)
--
-- Note that steps 1-4 can be carried out off-line and that the
-- only two steps needed to be done on-line are (a) computation
-- of H(idx) and (b) computation of Transpose(Base)*H(idx).
-- Clearly step (b) is obvious, so we will concentrate on (a).
--
-- In order to carry out step (a) we need a way to enumerat
-- _algorithmically_ the elements of G (or of H).
--
-- It is possible to show that one can find subsets S_k of G
-- (where S_k has order_k elements) such that each element
-- g of G can be written in one and only one way as
--
--   g = u_1(c_1) u_2(c_2) ...
--
-- where u_k(c_k) is the c_k element of S_k.  In the groups
-- used so far (H3 and F4) we always found, for every S_k,
-- a "generator" s_k such that
--
--       u_k(c_k) = s_k**(c_k-1)
--
-- but so far we do not know if this is a general result.
--
-- A rough description of the algorithm we are going to employ is
-- as follows:
--
--   1. Decompose Idx as
--
--        Idx = c_1 + order_1*c_2 + order_2*order_1*c_3 + ...
--
--   2. Compute
--
--        Base*u_1(c_1)*...
--
-- Of course, since we are interested in generating the element of H
-- only, not every sequence c_1, c_2, ... will be a valid one.  So far
-- (for groups H3 and F4) the set of valid Idx can be written as
-- K*idx, 0 <= idx < size(H) and suitable K.  Again, we do not know if
-- this is a general result.
--

package body Reduction_Vectors is
   Default_Pool : array(1..4) of Pool_Name :=
     (1 => No_Pool,
      2 => No_Pool,
      3 => (Class => 'H', Dim => 3, pool => 0),
      4 => (Class => 'F', Dim => 4, pool => 0));

   package Group_Maps is
      new Ada.Containers.Ordered_Maps(Key_Type     => Group_Name,
                                      Element_Type => Group_Descriptor_Pt);

   Group_Table : Group_Maps.Map;

   -- Since  the group does not change during a
   -- typical session, it makes sense to cache the requests
   -- to Group_Table.  Current_Group will contain the result
   -- of the last query.

   package Pool_Maps is
      new Ada.Containers.Ordered_Maps(Key_Type     => Pool_Name,
                                      Element_Type => Pool_Descriptor_Pt);

   Pool_Table : Pool_Maps.Map;

   -- Since  the pool does not change during a
   -- typical session, it makes sense to cache the requests
   -- to Pool_Table.  Current_Pool will contain the result
   -- of the last query.
   Current_Pool : access Pool_Descriptor := null;

   --
   -- Since in a typical session only a handful of reduction
   -- vectors is used, it is convenient to keep the already
   -- computed vectors in a cache.  Actually, I carried out
   -- some rough experiments to measure the difference in
   -- efficiency and I found out that with the cache we are
   -- almost 9 (!) times faster.
   --
   type Vector_Name is record  -- Used as the cache key
      Pool : Pool_Name;
      Idx  : Natural;
   end record;

   function "<" (X, Y: Vector_Name) return Boolean is
   begin
      -- With this approach we resort to the (more expensive)
      -- comparison between the pool names only when the
      -- field Idx are equal
      if (X.Idx > Y.Idx) then
         return False;
      elsif (X.Idx < Y.Idx) then
         return True;
      else
         return X.Pool < Y.Pool;
      end if;
   end "<";

   package Vector_Maps is
      new Ada.Containers.Ordered_Maps(Key_Type     => Vector_Name,
                                      Element_Type => Float_Matrix);

   Vector_Cache : Vector_Maps.Map;

   use type Vector_Maps.Cursor;

   --------------------------
   -- Get_Reduction_Vector --
   --------------------------

   function Get_Reduction_Vector
     (Idx  : Analogic_Vector_Idx;
      Pool : Pool_Name) return Float_Matrix is
      Pos : Vector_Maps.Cursor;
   begin
      -- First check that the request makes sense
      if (Pool = No_Pool) then
         raise Constraint_Error;
      end if;

      -- Check if we need to update the cache
      if (Current_Pool = null or else Pool /= Current_Pool.Name) then
         Current_Pool  := Pool_Table.Element(Pool);
      end if;

      -- Here Current_Pool contains the (reference to the) desired
      -- pool descriptor.  Check that Idx holds a valid index, i.e,
      -- non-negative (that is automatic) and smaller than the
      -- pool size
      if (Idx >= Current_Pool.Size) then
         raise Constraint_Error;
      end if;

      -- Check if the vector is already in the cache
      Pos := Vector_Cache.Find((Idx => Idx,
                                Pool => Current_Pool.Name));

      if (Pos /= Vector_Maps.No_Element) then
         -- Vector found in the cache: return it
         return Vector_Maps.Element(Pos);
      end if;

      -- Now we can compute the desired vector
      declare
         Result : Float_Matrix := Current_Pool.Base;
         Current_Idx : Natural := Idx*Current_Pool.Skip;
         Exp : Natural;
         Current_Group : access Group_Descriptor := Current_Pool.Group;
      begin
         for Step in Current_Group.Steps'Range loop
            Exp := Current_Idx mod Current_Group.Steps(Step).Order;
            Current_Idx := (Current_Idx-Exp) / Current_Group.Steps(Step).Order;

            for I in 1..Exp loop
               Result := Result*Current_Group.Steps(Step).Generator;
            end loop;

            -- If Current_Idx becomes null, we can skip the
            -- remaining part of the loop.
            exit when Current_Idx = 0;
         end loop;

         -- Add the new vector to the cache
         Vector_Cache.Insert(Key => (Idx => Idx, Pool => Current_Pool.Name),
                             New_Item => Result);

         return Result;
      end;
   end Get_Reduction_Vector;

   function Get_Reduction_Vector
     (Idx      : Analogic_Vector_Idx;
      Sampling : Positive)
      return Float_Matrix
   is
   begin
      return Get_Reduction_Vector (Idx, Default_Pool(Sampling));
   end Get_Reduction_Vector;
begin
   -- Package initialization

   -- First put the known groups into the group table
   for I in Known_Groups'Range loop
      Group_Table.Insert(Key => Known_Groups(I).Name,
                         New_Item => Known_Groups(I));
   end loop;

   --
   -- Now update the field "Group" of the known pools and
   -- write the known pools to the pool table
   --
   declare
      Name : Group_Name;
   begin
      for I in Known_Pools'Range loop
         Name := (Class => Known_Pools(I).Name.Class,
                  Dim   => Known_Pools(I).Name.Dim);
         Known_Pools(I).Group := Group_Table.Element(Name);

         Pool_Table.Insert(Key => Known_Pools(I).Name,
                           New_Item => Known_Pools(I));
      end loop;
   end;
end Reduction_Vectors;
