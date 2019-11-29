import os

import bpy

cam = bpy.data.objects['Camera']
origin = bpy.data.objects['Cube']

bpy.ops.object.select_all(action='DESELECT')
origin.select_set(True)
bpy.ops.object.delete()

#bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=4)
bpy.ops.mesh.primitive_uv_sphere_add()
bpy.ops.object.shade_smooth()

# bpy.data.materials.new('Prog mat')
bpy.context.object.active_material = bpy.data.materials['Material']
mat = bpy.context.object.active_material
image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
texture = mat.node_tree.nodes['Principled BSDF']
texture.inputs['Roughness'].default_value = 1
mat.node_tree.links.new(image_node.outputs['Color'], texture.inputs['Base Color'])

bpy.ops.image.open(filepath='/home/lemmih/Downloads/earth.jpg')
image_node.image = bpy.data.images['earth.jpg']



# image_node.image = bpy.data.images['earth.jpg']
# bpy.ops.image.open(filepath='/home/lemmih/Downloads/earth.jpg')
# image_node = mat.node_tree.nodes.new('ShaderNodeTexImage')
# image_node = mat.node_tree.nodes[1]
# texture = mat.node_tree.nodes[2]
# mat.node_tree.links.new(image_node.outputs[0], texture.inputs[0])
# bpy.data.materials.new('Prog mat')
# bpy.context.object.active_material = bpy.data.materials['Prog mat']

scn = bpy.context.scene
# scn.render.engine = 'CYCLES'
scn.render.film_transparent = True

bpy.data.scenes["Scene"].render.filepath = '/tmp/blender.png'
bpy.ops.render.render( write_still=True )
