import os
import math

import bpy

cam = bpy.data.objects['Camera']
origin = bpy.data.objects['Cube']

bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

#bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=4)
#bpy.ops.mesh.primitive_uv_sphere_add()
x = 0.9
bpy.ops.mesh.primitive_plane_add()
plane = bpy.context.object
plane.scale = (1,1.778,1)
plane.scale = (1,2,1)
bpy.ops.object.shade_smooth()
modifier = plane.modifiers.new(name='Subsurf', type='SUBSURF')
modifier.levels = 5
modifier.render_levels = 5
modifier.subdivision_type = 'SIMPLE'

bpy.ops.object.empty_add(type='ARROWS',rotation=(math.pi/2,0,0))
empty = bpy.context.object

bendUp = plane.modifiers.new(name='Bend up', type='SIMPLE_DEFORM')
bendUp.deform_method = 'BEND'
bendUp.origin = empty
bendUp.deform_axis = 'Z'
bendUp.factor = math.pi*x

bendAround = plane.modifiers.new(name='Bend up', type='SIMPLE_DEFORM')
bendAround.deform_method = 'BEND'
bendAround.origin = empty
bendAround.deform_axis = 'X'
bendAround.factor = math.pi*2*x

scn = bpy.context.scene
# scn.render.engine = 'CYCLES'
scn.render.film_transparent = True

bpy.ops.render.render( write_still=True )
