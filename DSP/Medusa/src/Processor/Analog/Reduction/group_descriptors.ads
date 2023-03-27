--                              -*- Mode: Ada -*-
--  Filename        : group_descriptors.ads
--  Description     : Description of the groups used to compute the red. vect.
--  Author          : Finta Tartaruga
--  Created On      : Tue Aug 21 13:08:08 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Basic testing on H3 and F4 OK.

--
-- This package contains the descriptions of the pool vector used to
-- obtain the reduction vectors and the corresponding groups.  Actually,
-- a separate package it was not really necessary, but we did in this
-- way in order not to clog reduction_vectors with the ugly
-- group descriptions.
--

with Text_Io;
use  Text_Io;

with Types.Float_Matrices;
use  Types.Float_Matrices;
use  Types.Float_Matrices.Float_Matrices_Pkg;

package Group_Descriptors is
   ----------------
   -- Group_Name --
   ----------------
   subtype Group_Class is Character range '@'..'Z';

   type Group_Name is record
      Class : Group_Class;
      Dim   : Natural;
   end record;


   function "<"(X, Y: Group_Name) return Boolean;
   function "="(X, Y: Group_Name) return Boolean;

   No_Group : constant Group_Name := (Class => '@', Dim => 0);

   ---------------------
   -- Step_Descriptor --
   ---------------------
   type Step_Descriptor is record
      Order : Positive;
      Generator : Float_Matrix;
   end record;

   type Step_Descriptor_Array is
     array(Positive range <>) of Step_Descriptor;

   ----------------------
   -- Group_Descriptor --
   ----------------------
   type Group_Descriptor is record
      Name : Group_Name;
      Sampling : Positive;
      Steps : access Step_Descriptor_Array;
   end record;

   type Group_Descriptor_Pt is access Group_Descriptor;

   type Group_Descriptor_Array is
     array(Positive range <>) of Group_Descriptor_Pt;

   ----------------------
   -- Pool descriptors --
   ----------------------

   type Pool_Name is record
      Class : Group_Class;
      Dim   : Natural;
      Pool  : Natural;
   end record;


   function "<"(X, Y: Pool_Name) return Boolean;
   function "="(X, Y: Pool_Name) return Boolean;

   No_Pool : constant Pool_Name := (Class => '@', Dim => 0, Pool => 0);

   type Pool_Descriptor is record
      Name  : Pool_Name;
      Group : Group_Descriptor_Pt;
      Size  : Positive;
      Base  : Float_Matrix;
      Skip  : Positive;
   end record;

   type Pool_Descriptor_Pt is access Pool_Descriptor;

   --------------------------------
   -- Descriptors of known pools --
   --------------------------------

   Known_Pools : array(1..2) of Pool_Descriptor_Pt :=
     (1 => new Pool_Descriptor'
      (Name => (Class => 'H', Dim => 3, Pool => 0),
       Group => null,  -- Filled at run-time
       Size  => 60,
       Skip  => 1,
       Base => Make_Row_Vector((1 => 0.26726124191,
                                2 => 0.53452248382,
                                3 => 0.80178372574))),
      2 => new Pool_Descriptor'
      (Name => (Class => 'F', Dim => 4, Pool => 0),
       Group => null,  -- Filled at run-time
       Size  => 576,
       Skip  => 2,
       Base => Make_Row_Vector((1 => 0.18257418584,
                                2 => 0.36514837167,
                                3 => 0.54772255751,
                                4 => 0.73029674334))));


   ---------------------------------
   -- Descriptors of known groups --
   ---------------------------------
   Known_Groups : Group_Descriptor_Array :=
     (1 => new Group_Descriptor'
        (Name     => (Class => 'H', Dim => 3),
         Sampling => 3,
         Steps    => new Step_Descriptor_Array'
         (1 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -1.00000000000, 2 => -0.00000000000, 3 =>  0.00000000000),
           2 => (1 =>  0.00000000000, 2 => -0.44721359550, 3 =>  0.89442719100),
           3 => (1 => -0.00000000000, 2 =>  0.89442719100, 3 =>  0.44721359550)))),
          2 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -1.00000000000, 2 => -0.00000000000, 3 =>  0.00000000000),
           2 => (1 =>  0.00000000000, 2 =>  0.44721359550, 3 => -0.89442719100),
           3 => (1 => -0.00000000000, 2 => -0.89442719100, 3 => -0.44721359550)))),
          3 => (Order => 3, Generator => Make_Matrix(
(          1 => (1 => -0.00000000000, 2 => -0.52573111212, 3 => -0.85065080835),
           2 => (1 =>  0.85065080835, 2 => -0.44721359550, 3 =>  0.27639320225),
           3 => (1 => -0.52573111212, 2 => -0.72360679775, 3 =>  0.44721359550)))),
          4 => (Order => 5, Generator => Make_Matrix(
(          1 => (1 =>  0.30901699437, 2 => -0.95105651630, 3 =>  0.00000000000),
           2 => (1 =>  0.95105651630, 2 =>  0.30901699437, 3 =>  0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  1.00000000000)))),
          5 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -1.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000),
           2 => (1 =>  0.00000000000, 2 =>  1.00000000000, 3 =>  0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  1.00000000000)))))),
      2 => new Group_Descriptor'
      (Name     => (Class => 'F', Dim => 4),
       Sampling => 4,
       Steps    => new Step_Descriptor_Array'
       (1 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -1.00000000000, 2 =>  0.00000000000, 3 => -0.00000000000, 4 => -0.00000000000),
           2 => (1 =>  0.00000000000, 2 => -1.00000000000, 3 => -0.00000000000, 4 => -0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 => -1.00000000000, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 => -0.00000000000, 4 => -1.00000000000)))),
        2 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 =>  0.00000000000, 2 => -0.57735026919, 3 =>  0.81649658093, 4 => -0.00000000000),
           2 => (1 =>  0.57735026919, 2 => -0.00000000000, 3 =>  0.00000000000, 4 =>  0.81649658093),
           3 => (1 => -0.81649658093, 2 => -0.00000000000, 3 => -0.00000000000, 4 =>  0.57735026919),
           4 => (1 =>  0.00000000000, 2 => -0.81649658093, 3 => -0.57735026919, 4 => -0.00000000000)))),
        3 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 =>  0.00000000000, 2 =>  0.57735026919, 3 =>  0.40824829046, 4 =>  0.70710678119),
           2 => (1 =>  0.57735026919, 2 =>  0.66666666667, 3 => -0.23570226040, 4 => -0.40824829046),
           3 => (1 =>  0.40824829046, 2 => -0.23570226040, 3 => -0.66666666667, 4 =>  0.57735026919),
           4 => (1 =>  0.70710678119, 2 => -0.40824829046, 3 =>  0.57735026919, 4 => -0.00000000000)))),
        4 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -0.00000000000, 2 =>  0.57735026919, 3 => -0.81649658093, 4 =>  0.00000000000),
           2 => (1 =>  0.57735026919, 2 => -0.66666666667, 3 => -0.47140452079, 4 =>  0.00000000000),
           3 => (1 => -0.81649658093, 2 => -0.47140452079, 3 => -0.33333333333, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  1.00000000000)))),
        5 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -0.00000000000, 2 => -0.57735026919, 3 =>  0.81649658093, 4 =>  0.00000000000),
           2 => (1 => -0.57735026919, 2 => -0.66666666667, 3 => -0.47140452079, 4 =>  0.00000000000),
           3 => (1 =>  0.81649658093, 2 => -0.47140452079, 3 => -0.33333333333, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  1.00000000000)))),
        6 => (Order => 3, Generator => Make_Matrix(
(          1 => (1 => -0.50000000000, 2 => -0.86602540378, 3 =>  0.00000000000, 4 =>  0.00000000000),
           2 => (1 =>  0.86602540378, 2 => -0.50000000000, 3 =>  0.00000000000, 4 =>  0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  1.00000000000, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  1.00000000000)))),
        7 => (Order => 3, Generator => Make_Matrix(
(          1 => (1 =>  1.00000000000, 2 => -0.00000000000, 3 => -0.00000000000, 4 => -0.00000000000),
           2 => (1 => -0.00000000000, 2 => -0.33333333333, 3 => -0.94280904158, 4 =>  0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.47140452079, 3 => -0.16666666667, 4 =>  0.86602540378),
           4 => (1 => -0.00000000000, 2 => -0.81649658093, 3 =>  0.28867513459, 4 =>  0.50000000000)))),
        8 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 => -1.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  0.00000000000),
           2 => (1 =>  0.00000000000, 2 =>  1.00000000000, 3 =>  0.00000000000, 4 =>  0.00000000000),
           3 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  1.00000000000, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  1.00000000000)))),
        9 => (Order => 2, Generator => Make_Matrix(
(          1 => (1 =>  1.00000000000, 2 => -0.00000000000, 3 => -0.00000000000, 4 =>  0.00000000000),
           2 => (1 => -0.00000000000, 2 => -0.33333333333, 3 => -0.94280904158, 4 =>  0.00000000000),
           3 => (1 => -0.00000000000, 2 => -0.94280904158, 3 =>  0.33333333333, 4 =>  0.00000000000),
           4 => (1 =>  0.00000000000, 2 =>  0.00000000000, 3 =>  0.00000000000, 4 =>  1.00000000000))))))
      ); -- Known groups


end Group_Descriptors;
