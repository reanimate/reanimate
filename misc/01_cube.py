# blender --background --python 01_cube.py -- </path/to/output/image> <resolution_percentage>

import bpy
import os
import sys
import math

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import utils


def get_output_file_path() -> str:
    return str(sys.argv[sys.argv.index('--') + 1]) 


def get_resolution_percentage() -> int:
    return int(sys.argv[sys.argv.index('--') + 2])


if __name__ == "__main__":
    # Args
    output_file_path = get_output_file_path()
    resolution_percentage = get_resolution_percentage()

    # Setting
    default_scene = bpy.context.scene
    default_camera_object = bpy.data.objects["Camera"]

    bpy.ops.object.empty_add(location=(0.0, 0, 0.0))
    focus_target = bpy.context.object
    bpy.ops.object.select_all(action='DESELECT')
    default_camera_object.select_set(True)
    focus_target.select_set(True)
    bpy.ops.object.parent_set()
    focus_target.rotation_mode = 'XYZ'
    focus_target.rotation_euler = (0,0,0)
    focus_target.keyframe_insert(data_path='rotation_euler', frame=1)
    focus_target.rotation_euler = (0,0,math.pi*2)
    focus_target.keyframe_insert(data_path='rotation_euler', frame=120)

    for k in focus_target.animation_data.action.fcurves.find('rotation_euler', index=2).keyframe_points:
        k.interpolation = 'LINEAR'
    

    num_samples = 32

    utils.set_animation(default_scene, fps=60, frame_start=1, frame_end=120)

    utils.set_cycles_renderer(default_scene, resolution_percentage, output_file_path, default_camera_object,
                              num_samples, use_denoising=True)

    # Rendering
    bpy.ops.render.render(animation=True, write_still=True)
