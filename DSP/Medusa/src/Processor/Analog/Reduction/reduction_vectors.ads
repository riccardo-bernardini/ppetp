--                              -*- Mode: Ada -*-
--  Filename        : reduction_vectors.ads
--  Description     : Compute the reduction vectors
--  Author          : Finta Tartaruga
--  Created On      : Tue Aug 21 13:06:49 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : 0.0 Basic tests with H3 and F4: ok

--
-- A vector of analogic data is reduced by partitioning it into
-- blocks of size Sampling and multiplying each block by a
-- "reduction vector".  In order to make reconstruction possible,
-- it is necessary to transmit a description of the reduction
-- vector together with the reduced data.
--
-- In order to transmit the reduction vector "precisely", without
-- the error due to a possible quantization, the reduction vector
-- is extracted from a vector pool, indexed by a variable of
-- type Analogic_Vector_Idx.  The used index is sent
-- with the reduced data.
--
-- This package exports a function which maps any (sampling, index)
-- pair into the corresponding reduction vector.
--

with Text_Io;
use  Text_Io;

with Types.Packets.Uniform.Analogic;
use  Types.Packets.Uniform.Analogic;

with Types.Float_Matrices;
use  Types.Float_Matrices;
use  Types.Float_Matrices.Float_Matrices_Pkg;

package Reduction_Vectors is
   function Get_Reduction_Vector (Idx      : Analogic_Vector_Idx;
                                  Sampling : Positive) return Float_Matrix;

end Reduction_Vectors;
