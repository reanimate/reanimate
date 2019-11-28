import bpy
import mathutils
from utils.mesh import create_mesh_from_pydata
from utils.modifier import add_subdivision_surface_modifier
from typing import Any, Dict, Iterable, List, Tuple


def create_armature_mesh(scene: bpy.types.Scene, armature_object: bpy.types.Object, mesh_name: str) -> bpy.types.Object:
    assert armature_object.type == 'ARMATURE', 'Error'
    assert len(armature_object.data.bones) != 0, 'Error'

    def add_rigid_vertex_group(target_object: bpy.types.Object, name: str, vertex_indices: Iterable[int]) -> None:
        new_vertex_group = target_object.vertex_groups.new(name=name)
        for vertex_index in vertex_indices:
            new_vertex_group.add([vertex_index], 1.0, 'REPLACE')

    def generate_bone_mesh_pydata(radius: float, length: float) -> Tuple[List[mathutils.Vector], List[List[int]]]:
        base_radius = radius
        top_radius = 0.5 * radius

        vertices = [
            # Cross section of the base part
            mathutils.Vector((-base_radius, 0.0, +base_radius)),
            mathutils.Vector((+base_radius, 0.0, +base_radius)),
            mathutils.Vector((+base_radius, 0.0, -base_radius)),
            mathutils.Vector((-base_radius, 0.0, -base_radius)),

            # Cross section of the top part
            mathutils.Vector((-top_radius, length, +top_radius)),
            mathutils.Vector((+top_radius, length, +top_radius)),
            mathutils.Vector((+top_radius, length, -top_radius)),
            mathutils.Vector((-top_radius, length, -top_radius)),

            # End points
            mathutils.Vector((0.0, -base_radius, 0.0)),
            mathutils.Vector((0.0, length + top_radius, 0.0))
        ]

        faces = [
            # End point for the base part
            [8, 1, 0],
            [8, 2, 1],
            [8, 3, 2],
            [8, 0, 3],

            # End point for the top part
            [9, 4, 5],
            [9, 5, 6],
            [9, 6, 7],
            [9, 7, 4],

            # Side faces
            [0, 1, 5, 4],
            [1, 2, 6, 5],
            [2, 3, 7, 6],
            [3, 0, 4, 7],
        ]

        return vertices, faces

    armature_data: bpy.types.Armature = armature_object.data

    vertices: List[mathutils.Vector] = []
    faces: List[List[int]] = []
    vertex_groups: List[Dict[str, Any]] = []

    for bone in armature_data.bones:
        radius = 0.10 * (0.10 + bone.length)
        temp_vertices, temp_faces = generate_bone_mesh_pydata(radius, bone.length)

        vertex_index_offset = len(vertices)

        temp_vertex_group = {'name': bone.name, 'vertex_indices': []}
        for local_index, vertex in enumerate(temp_vertices):
            if bpy.app.version >= (2, 80, 0):
                vertices.append(bone.matrix_local @ vertex)
            else:
                vertices.append(bone.matrix_local * vertex)
            temp_vertex_group['vertex_indices'].append(local_index + vertex_index_offset)
        vertex_groups.append(temp_vertex_group)

        for face in temp_faces:
            if len(face) == 3:
                faces.append([
                    face[0] + vertex_index_offset,
                    face[1] + vertex_index_offset,
                    face[2] + vertex_index_offset,
                ])
            else:
                faces.append([
                    face[0] + vertex_index_offset,
                    face[1] + vertex_index_offset,
                    face[2] + vertex_index_offset,
                    face[3] + vertex_index_offset,
                ])

    new_object = create_mesh_from_pydata(scene, vertices, faces, mesh_name, mesh_name)
    new_object.matrix_world = armature_object.matrix_world

    for vertex_group in vertex_groups:
        add_rigid_vertex_group(new_object, vertex_group['name'], vertex_group['vertex_indices'])

    armature_modifier = new_object.modifiers.new('Armature', 'ARMATURE')
    armature_modifier.object = armature_object
    armature_modifier.use_vertex_groups = True

    add_subdivision_surface_modifier(new_object, 1, is_simple=True)
    add_subdivision_surface_modifier(new_object, 2, is_simple=False)

    # Set the armature as the parent of the new object
    bpy.ops.object.select_all(action='DESELECT')
    if bpy.app.version >= (2, 80, 0):
        new_object.select_set(True)
        armature_object.select_set(True)
        bpy.context.view_layer.objects.active = armature_object
    else:
        new_object.select = True
        armature_object.select = True
        bpy.context.scene.objects.active = armature_object
    bpy.ops.object.parent_set(type='OBJECT')

    return new_object
