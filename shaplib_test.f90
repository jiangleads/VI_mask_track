	!    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>
	!
	!    This file is part of FortranGIS.
	!
	!    FortranGIS is free software: you can redistribute it and/or modify
	!    it under the terms of the GNU Lesser General Public License as
	!    published by the Free Software Foundation, either version 3 of the
	!    License, or (at your option) any later version.
	!
	!    FortranGIS is distributed in the hope that it will be useful, but
	!    WITHOUT ANY WARRANTY; without even the implied warranty of
	!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	!    Lesser General Public License for more details.
	!
	!    You should have received a copy of the GNU Lesser General Public
	!    License along with FortranGIS.  If not, see
	!    <http://www.gnu.org/licenses/>.
	PROGRAM shapelib_test
	use,INTRINSIC :: iso_c_binding
	USE shapelib
	IMPLICIT NONE
	INTEGER,PARAMETER :: lencharattr=40, nshp=4, tshp=shpt_polygonz
	TYPE(shpfileobject) :: shphandle
	TYPE(shpobject) :: shpobj
	INTEGER :: i, j
	CHARACTER(len=1024) :: filename
	INTEGER :: nshpr, tshpr, nfield, nrec, nd
	REAL(kind=c_double) :: minbound(4), maxbound(4)
	CHARACTER(len=lencharattr) :: charattrr
	INTEGER :: intattrr
	REAL(kind=c_double) :: doubleattrr
	!CALL getarg(1,filename)
	!IF (filename == '') THEN
	!  PRINT'(A)','Usage: shape_test <shp_file>'
	!  STOP
	!ENDIF
	filename = '../../../Downloads/shapfiles/bou2_4m/bou2_4l'
	! ==== How to read a shapefile ====
	! open an exixting shapefile and associate it to a shapefile object
	! filename does not include extension
	shphandle = shpopen(trim(filename), 'rb')
	! error check
	IF (shpfileisnull(shphandle) .OR. dbffileisnull(shphandle)) THEN
		print*,'Error opening ',trim(filename),' for reading'
		stop 1
	ENDIF
	! get general information about the shapefile object
	CALL shpgetinfo(shphandle, nshpr, tshpr, minbound, maxbound, nfield, nrec)

	! read the nshp shapes
	DO i = 0, nshp - 1
		print*,'Checking shape',i
		! read the i-th shape from the shapefile object and obtain a shape object
		shpobj = shpreadobject(shphandle, i)
		! error check
		IF (shpisnull(shpobj)) THEN
			print*,'Error in shpreadobject',i
			stop 1
		ENDIF
		! now access all the components of the shape object
		! number of vertices
		print*,"number of vertices",shpobj%nvertices

		! x shape data
		print*,'x shape data',shpobj%padfx(:)

		! y shape data
		print*,'y shape data',shpobj%padfy(:)

		! z shape data
		print*,'z shape data',shpobj%padfy(:)

		! destroy the shape object to avoid memory leaks
		! notice that for accessing dbf attributes the shape object is not required
		CALL shpdestroyobject(shpobj)
		! get the dbf attributes for the i-th shape in the shapefile object
		! first field (character)
		CALL dbfreadattribute(shphandle, i, 0, charattrr)
		print*,charattrr

		! second field (integer)
		CALL dbfreadattribute(shphandle, i, 1, intattrr)
		print*,intattrr

		! third field (double)
		CALL dbfreadattribute(shphandle, i, 2, doubleattrr)
		print*,doubleattrr

	ENDDO
	! close the shapefile object
	CALL shpclose(shphandle)


	END PROGRAM shapelib_test

