import bpy
from typing import Tuple
from utils.node import arrange_nodes

################################################################################
# Scene
################################################################################


def set_animation(scene: bpy.types.Scene,
                  fps: int = 24,
                  frame_start: int = 1,
                  frame_end: int = 48,
                  frame_current: int = 1) -> None:
    scene.render.fps = fps
    scene.frame_start = frame_start
    scene.frame_end = frame_end
    scene.frame_current = frame_current


def build_rgb_background(world: bpy.types.World,
                         rgb: Tuple[float, float, float, float] = (0.9, 0.9, 0.9, 1.0),
                         strength: float = 1.0) -> None:
    world.use_nodes = True
    node_tree = world.node_tree

    rgb_node = node_tree.nodes.new(type="ShaderNodeRGB")
    rgb_node.outputs["Color"].default_value = rgb

    node_tree.nodes["Background"].inputs["Strength"].default_value = strength

    node_tree.links.new(rgb_node.outputs["Color"], node_tree.nodes["Background"].inputs["Color"])

    arrange_nodes(node_tree)


def build_environment_texture_background(world: bpy.types.World, hdri_path: str, rotation: float = 0.0) -> None:
    world.use_nodes = True
    node_tree = world.node_tree

    environment_texture_node = node_tree.nodes.new(type="ShaderNodeTexEnvironment")
    environment_texture_node.image = bpy.data.images.load(hdri_path)

    mapping_node = node_tree.nodes.new(type="ShaderNodeMapping")
    mapping_node.rotation[2] = rotation

    tex_coord_node = node_tree.nodes.new(type="ShaderNodeTexCoord")

    node_tree.links.new(tex_coord_node.outputs["Generated"], mapping_node.inputs["Vector"])
    node_tree.links.new(mapping_node.outputs["Vector"], environment_texture_node.inputs["Vector"])
    node_tree.links.new(environment_texture_node.outputs["Color"], node_tree.nodes["Background"].inputs["Color"])

    arrange_nodes(node_tree)


def set_cycles_renderer(scene: bpy.types.Scene,
                        resolution_percentage: int,
                        output_file_path: str,
                        camera_object: bpy.types.Object,
                        num_samples: int,
                        use_denoising: bool = True,
                        use_motion_blur: bool = False,
                        use_transparent_bg: bool = False) -> None:
    scene.camera = camera_object

    #scene.render.image_settings.file_format = 'PNG'
    scene.render.resolution_percentage = resolution_percentage
    scene.render.engine = 'CYCLES'
    #scene.render.filepath = output_file_path
    scene.render.use_motion_blur = use_motion_blur

    if bpy.app.version >= (2, 80, 0):
        scene.render.film_transparent = use_transparent_bg
        scene.view_layers[0].cycles.use_denoising = use_denoising
    else:
        scene.cycles.film_transparent = use_transparent_bg
        scene.render.layers[0].cycles.use_denoising = use_denoising

    scene.cycles.samples = num_samples


################################################################################
# Constraints
################################################################################


def add_track_to_constraint(camera_object: bpy.types.Object, track_to_target_object: bpy.types.Object) -> None:
    constraint = camera_object.constraints.new(type='TRACK_TO')
    constraint.target = track_to_target_object
    constraint.track_axis = 'TRACK_NEGATIVE_Z'
    constraint.up_axis = 'UP_Y'


def add_copy_location_constraint(copy_to_object: bpy.types.Object,
                                 copy_from_object: bpy.types.Object,
                                 use_x: bool,
                                 use_y: bool,
                                 use_z: bool,
                                 bone_name: str = '') -> None:
    constraint = copy_to_object.constraints.new(type='COPY_LOCATION')
    constraint.target = copy_from_object
    constraint.use_x = use_x
    constraint.use_y = use_y
    constraint.use_z = use_z
    if bone_name:
        constraint.subtarget = bone_name


################################################################################
# Misc.
################################################################################


def clean_objects() -> None:
    for item in bpy.data.objects:
        bpy.data.objects.remove(item)
