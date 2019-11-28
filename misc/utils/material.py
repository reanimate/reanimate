import bpy
from typing import Tuple
from utils.node import set_socket_value_range, arrange_nodes


def create_texture_node(node_tree: bpy.types.NodeTree, path: str, is_color_data: bool) -> bpy.types.Node:
    # Instantiate a new texture image node
    texture_node = node_tree.nodes.new(type='ShaderNodeTexImage')

    # Open an image and set it to the node
    texture_node.image = bpy.data.images.load(path)

    # Set other parameters
    if bpy.app.version >= (2, 80, 0):
        texture_node.image.colorspace_settings.is_data = False if is_color_data else True
    else:
        texture_node.color_space = 'COLOR' if is_color_data else 'NONE'

    # Return the node
    return texture_node


def set_principled_node(principled_node,
                        base_color=(0.6, 0.6, 0.6, 1.0),
                        subsurface=0.0,
                        subsurface_color=(0.8, 0.8, 0.8, 1.0),
                        subsurface_radius=(1.0, 0.2, 0.1),
                        metallic=0.0,
                        specular=0.5,
                        specular_tint=0.0,
                        roughness=0.5,
                        anisotropic=0.0,
                        anisotropic_rotation=0.0,
                        sheen=0.0,
                        sheen_tint=0.5,
                        clearcoat=0.0,
                        clearcoat_roughness=0.03,
                        ior=1.45,
                        transmission=0.0,
                        transmission_roughness=0.0):
    principled_node.inputs['Base Color'].default_value = base_color
    principled_node.inputs['Subsurface'].default_value = subsurface
    principled_node.inputs['Subsurface Color'].default_value = subsurface_color
    principled_node.inputs['Subsurface Radius'].default_value = subsurface_radius
    principled_node.inputs['Metallic'].default_value = metallic
    principled_node.inputs['Specular'].default_value = specular
    principled_node.inputs['Specular Tint'].default_value = specular_tint
    principled_node.inputs['Roughness'].default_value = roughness
    principled_node.inputs['Anisotropic'].default_value = anisotropic
    principled_node.inputs['Anisotropic Rotation'].default_value = anisotropic_rotation
    principled_node.inputs['Sheen'].default_value = sheen
    principled_node.inputs['Sheen Tint'].default_value = sheen_tint
    principled_node.inputs['Clearcoat'].default_value = clearcoat
    principled_node.inputs['Clearcoat Roughness'].default_value = clearcoat_roughness
    principled_node.inputs['IOR'].default_value = ior
    principled_node.inputs['Transmission'].default_value = transmission
    principled_node.inputs['Transmission Roughness'].default_value = transmission_roughness


def build_pbr_nodes(node_tree: bpy.types.NodeTree,
                    base_color: Tuple[float, float, float, float] = (0.6, 0.6, 0.6, 1.0),
                    metallic: float = 0.0,
                    specular: float = 0.5,
                    roughness: float = 0.5,
                    sheen: float = 0.0) -> None:
    output_node = node_tree.nodes.new(type='ShaderNodeOutputMaterial')
    principled_node = node_tree.nodes.new(type='ShaderNodeBsdfPrincipled')
    node_tree.links.new(principled_node.outputs['BSDF'], output_node.inputs['Surface'])

    set_principled_node(principled_node=principled_node,
                        base_color=base_color,
                        metallic=metallic,
                        specular=specular,
                        roughness=roughness,
                        sheen=sheen)

    arrange_nodes(node_tree)


def build_checker_board_nodes(node_tree: bpy.types.NodeTree, size: float) -> None:
    output_node = node_tree.nodes.new(type='ShaderNodeOutputMaterial')
    principled_node = node_tree.nodes.new(type='ShaderNodeBsdfPrincipled')
    checker_texture_node = node_tree.nodes.new(type='ShaderNodeTexChecker')

    set_principled_node(principled_node=principled_node)
    checker_texture_node.inputs['Scale'].default_value = size

    node_tree.links.new(checker_texture_node.outputs['Color'], principled_node.inputs['Base Color'])
    node_tree.links.new(principled_node.outputs['BSDF'], output_node.inputs['Surface'])

    arrange_nodes(node_tree)


def build_matcap_nodes(node_tree: bpy.types.NodeTree, image_path: str) -> None:
    tex_coord_node = node_tree.nodes.new(type='ShaderNodeTexCoord')
    vector_transform_node = node_tree.nodes.new(type='ShaderNodeVectorTransform')
    mapping_node = node_tree.nodes.new(type='ShaderNodeMapping')
    texture_image_node = create_texture_node(node_tree, image_path, True)
    emmission_node = node_tree.nodes.new(type='ShaderNodeEmission')
    output_node = node_tree.nodes.new(type='ShaderNodeOutputMaterial')

    frame = node_tree.nodes.new(type='NodeFrame')
    frame.name = "MatCap UV"
    frame.label = "MatCap UV"
    tex_coord_node.parent = frame
    vector_transform_node.parent = frame
    mapping_node.parent = frame

    vector_transform_node.vector_type = "VECTOR"
    vector_transform_node.convert_from = "OBJECT"
    vector_transform_node.convert_to = "CAMERA"

    mapping_node.vector_type = "TEXTURE"
    mapping_node.translation = (1.0, 1.0, 0.0)
    mapping_node.scale = (2.0, 2.0, 1.0)

    node_tree.links.new(tex_coord_node.outputs['Normal'], vector_transform_node.inputs['Vector'])
    node_tree.links.new(vector_transform_node.outputs['Vector'], mapping_node.inputs['Vector'])
    node_tree.links.new(mapping_node.outputs['Vector'], texture_image_node.inputs['Vector'])
    node_tree.links.new(texture_image_node.outputs['Color'], emmission_node.inputs['Color'])
    node_tree.links.new(emmission_node.outputs['Emission'], output_node.inputs['Surface'])

    arrange_nodes(node_tree)


def build_pbr_textured_nodes(node_tree: bpy.types.NodeTree,
                             color_texture_path: str = "",
                             metallic_texture_path: str = "",
                             roughness_texture_path: str = "",
                             normal_texture_path: str = "",
                             displacement_texture_path: str = "",
                             ambient_occlusion_texture_path: str = "",
                             scale: Tuple[float, float, float] = (1.0, 1.0, 1.0)) -> None:
    output_node = node_tree.nodes.new(type='ShaderNodeOutputMaterial')
    principled_node = node_tree.nodes.new(type='ShaderNodeBsdfPrincipled')
    node_tree.links.new(principled_node.outputs['BSDF'], output_node.inputs['Surface'])

    coord_node = node_tree.nodes.new(type='ShaderNodeTexCoord')
    mapping_node = node_tree.nodes.new(type='ShaderNodeMapping')
    mapping_node.vector_type = 'TEXTURE'
    mapping_node.scale = scale
    node_tree.links.new(coord_node.outputs['UV'], mapping_node.inputs['Vector'])

    if color_texture_path != "":
        texture_node = create_texture_node(node_tree, color_texture_path, True)
        node_tree.links.new(mapping_node.outputs['Vector'], texture_node.inputs['Vector'])
        if ambient_occlusion_texture_path != "":
            ao_texture_node = create_texture_node(node_tree, ambient_occlusion_texture_path, False)
            node_tree.links.new(mapping_node.outputs['Vector'], ao_texture_node.inputs['Vector'])
            mix_node = node_tree.nodes.new(type='ShaderNodeMixRGB')
            mix_node.blend_type = 'MULTIPLY'
            node_tree.links.new(texture_node.outputs['Color'], mix_node.inputs['Color1'])
            node_tree.links.new(ao_texture_node.outputs['Color'], mix_node.inputs['Color2'])
            node_tree.links.new(mix_node.outputs['Color'], principled_node.inputs['Base Color'])
        else:
            node_tree.links.new(texture_node.outputs['Color'], principled_node.inputs['Base Color'])

    if metallic_texture_path != "":
        texture_node = create_texture_node(node_tree, metallic_texture_path, False)
        node_tree.links.new(mapping_node.outputs['Vector'], texture_node.inputs['Vector'])
        node_tree.links.new(texture_node.outputs['Color'], principled_node.inputs['Metallic'])

    if roughness_texture_path != "":
        texture_node = create_texture_node(node_tree, roughness_texture_path, False)
        node_tree.links.new(mapping_node.outputs['Vector'], texture_node.inputs['Vector'])
        node_tree.links.new(texture_node.outputs['Color'], principled_node.inputs['Roughness'])

    if normal_texture_path != "":
        texture_node = create_texture_node(node_tree, normal_texture_path, False)
        node_tree.links.new(mapping_node.outputs['Vector'], texture_node.inputs['Vector'])
        normal_map_node = node_tree.nodes.new(type='ShaderNodeNormalMap')
        node_tree.links.new(texture_node.outputs['Color'], normal_map_node.inputs['Color'])
        node_tree.links.new(normal_map_node.outputs['Normal'], principled_node.inputs['Normal'])

    if displacement_texture_path != "":
        texture_node = create_texture_node(node_tree, displacement_texture_path, False)
        node_tree.links.new(mapping_node.outputs['Vector'], texture_node.inputs['Vector'])
        node_tree.links.new(texture_node.outputs['Color'], output_node.inputs['Displacement'])

    arrange_nodes(node_tree)


def add_parametric_color_ramp() -> bpy.types.NodeGroup:
    group = bpy.data.node_groups.new(type="ShaderNodeTree", name="Parametric Color Ramp")

    # Input

    input_node = group.nodes.new(type="NodeGroupInput")
    group.inputs.new("NodeSocketFloatFactor", "Fac")
    group.inputs.new("NodeSocketColor", "Color1")
    group.inputs.new("NodeSocketColor", "Color2")
    group.inputs.new("NodeSocketFloatFactor", "Pos1")
    group.inputs.new("NodeSocketFloatFactor", "Pos2")

    set_socket_value_range(group.inputs["Fac"], default_value=0.5)
    set_socket_value_range(group.inputs["Pos1"], default_value=0.0)
    set_socket_value_range(group.inputs["Pos2"], default_value=1.0)

    # Math

    denominator_subtract_node = group.nodes.new(type="ShaderNodeMath")
    denominator_subtract_node.operation = "SUBTRACT"
    denominator_subtract_node.use_clamp = True

    numerator_subtract_node = group.nodes.new(type="ShaderNodeMath")
    numerator_subtract_node.operation = "SUBTRACT"
    numerator_subtract_node.use_clamp = True

    divide_node = group.nodes.new(type="ShaderNodeMath")
    divide_node.operation = "DIVIDE"
    divide_node.use_clamp = True

    group.links.new(input_node.outputs["Pos2"], denominator_subtract_node.inputs[0])
    group.links.new(input_node.outputs["Fac"], denominator_subtract_node.inputs[1])

    group.links.new(input_node.outputs["Pos2"], numerator_subtract_node.inputs[0])
    group.links.new(input_node.outputs["Pos1"], numerator_subtract_node.inputs[1])

    group.links.new(denominator_subtract_node.outputs["Value"], divide_node.inputs[0])
    group.links.new(numerator_subtract_node.outputs["Value"], divide_node.inputs[1])

    # Mixing

    mix_node = group.nodes.new(type="ShaderNodeMixRGB")

    group.links.new(divide_node.outputs["Value"], mix_node.inputs["Fac"])
    group.links.new(input_node.outputs["Color2"], mix_node.inputs[1])
    group.links.new(input_node.outputs["Color1"], mix_node.inputs[2])

    # Output

    output_node = group.nodes.new(type="NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Color")

    group.links.new(mix_node.outputs["Color"], output_node.inputs["Color"])

    # Return

    arrange_nodes(group)

    return group


def create_parametric_color_ramp_node(node_tree: bpy.types.NodeTree) -> bpy.types.Node:
    color_ramp_node_group: bpy.types.NodeGroup

    if "Parametric Color Ramp" in bpy.data.node_groups:
        color_ramp_node_group = bpy.data.node_groups["Parametric Color Ramp"]
    else:
        color_ramp_node_group = add_parametric_color_ramp()

    node = node_tree.nodes.new(type='ShaderNodeGroup')
    node.name = "Parametric Color Ramp"
    node.node_tree = color_ramp_node_group

    return node


def add_tri_parametric_color_ramp() -> bpy.types.NodeGroup:
    group = bpy.data.node_groups.new(type="ShaderNodeTree", name="Tri Parametric Color Ramp")

    # Input

    input_node = group.nodes.new(type="NodeGroupInput")
    group.inputs.new("NodeSocketFloatFactor", "Fac")
    group.inputs.new("NodeSocketColor", "Color1")
    group.inputs.new("NodeSocketColor", "Color2")
    group.inputs.new("NodeSocketColor", "Color3")
    group.inputs.new("NodeSocketFloatFactor", "Pos1")
    group.inputs.new("NodeSocketFloatFactor", "Pos2")
    group.inputs.new("NodeSocketFloatFactor", "Pos3")

    set_socket_value_range(group.inputs["Fac"], default_value=0.5)
    set_socket_value_range(group.inputs["Pos1"], default_value=0.25)
    set_socket_value_range(group.inputs["Pos2"], default_value=0.50)
    set_socket_value_range(group.inputs["Pos3"], default_value=0.75)

    # Nested color ramp

    nested_color_ramp_node = create_parametric_color_ramp_node(group)

    group.links.new(input_node.outputs["Color1"], nested_color_ramp_node.inputs["Color1"])
    group.links.new(input_node.outputs["Color2"], nested_color_ramp_node.inputs["Color2"])
    group.links.new(input_node.outputs["Pos1"], nested_color_ramp_node.inputs["Pos1"])
    group.links.new(input_node.outputs["Pos2"], nested_color_ramp_node.inputs["Pos2"])
    group.links.new(input_node.outputs["Fac"], nested_color_ramp_node.inputs["Fac"])

    # Math

    denominator_subtract_node = group.nodes.new(type="ShaderNodeMath")
    denominator_subtract_node.operation = "SUBTRACT"
    denominator_subtract_node.use_clamp = True

    numerator_subtract_node = group.nodes.new(type="ShaderNodeMath")
    numerator_subtract_node.operation = "SUBTRACT"
    numerator_subtract_node.use_clamp = True

    divide_node = group.nodes.new(type="ShaderNodeMath")
    divide_node.operation = "DIVIDE"
    divide_node.use_clamp = True

    group.links.new(input_node.outputs["Pos3"], denominator_subtract_node.inputs[0])
    group.links.new(input_node.outputs["Fac"], denominator_subtract_node.inputs[1])

    group.links.new(input_node.outputs["Pos3"], numerator_subtract_node.inputs[0])
    group.links.new(input_node.outputs["Pos2"], numerator_subtract_node.inputs[1])

    group.links.new(denominator_subtract_node.outputs["Value"], divide_node.inputs[0])
    group.links.new(numerator_subtract_node.outputs["Value"], divide_node.inputs[1])

    # Mixing

    mix_node = group.nodes.new(type="ShaderNodeMixRGB")

    group.links.new(divide_node.outputs["Value"], mix_node.inputs["Fac"])
    group.links.new(input_node.outputs["Color3"], mix_node.inputs[1])
    group.links.new(nested_color_ramp_node.outputs["Color"], mix_node.inputs[2])

    # Output

    output_node = group.nodes.new(type="NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Color")

    group.links.new(mix_node.outputs["Color"], output_node.inputs["Color"])

    # Return

    arrange_nodes(group)

    return group


def create_tri_parametric_color_ramp_node(node_tree: bpy.types.NodeTree) -> bpy.types.Node:
    tri_color_ramp_node_group: bpy.types.NodeGroup

    if "Tri Parametric Color Ramp" in bpy.data.node_groups:
        tri_color_ramp_node_group = bpy.data.node_groups["Tri Parametric Color Ramp"]
    else:
        tri_color_ramp_node_group = add_tri_parametric_color_ramp()

    node = node_tree.nodes.new(type='ShaderNodeGroup')
    node.name = "Tri Parametric Color Ramp"
    node.node_tree = tri_color_ramp_node_group

    return node


def add_peeling_paint_metal_node_group() -> bpy.types.NodeGroup:
    group = bpy.data.node_groups.new(type="ShaderNodeTree", name="Peeling Paint Metal")

    input_node = group.nodes.new(type="NodeGroupInput")
    group.inputs.new("NodeSocketColor", "Paint Color")
    group.inputs.new("NodeSocketColor", "Metal Color")
    group.inputs.new("NodeSocketFloat", "Scale")
    group.inputs.new("NodeSocketFloat", "Detail")
    group.inputs.new("NodeSocketFloat", "Distortion")
    group.inputs.new("NodeSocketFloatFactor", "Threshold")

    set_socket_value_range(group.inputs["Scale"], default_value=4.5, min_value=0.0, max_value=1000.0)
    set_socket_value_range(group.inputs["Detail"], default_value=8.0, min_value=0.0, max_value=16.0)
    set_socket_value_range(group.inputs["Distortion"], default_value=0.5, min_value=0.0, max_value=1000.0)
    set_socket_value_range(group.inputs["Threshold"], default_value=0.42)

    group.inputs["Paint Color"].default_value = (0.152, 0.524, 0.067, 1.000)
    group.inputs["Metal Color"].default_value = (0.062, 0.015, 0.011, 1.000)

    tex_coord_node = group.nodes.new(type="ShaderNodeTexCoord")
    mapping_node = group.nodes.new(type="ShaderNodeMapping")

    group.links.new(tex_coord_node.outputs["Object"], mapping_node.inputs["Vector"])

    # Peeling region segmentation

    peeling_noise_node = group.nodes.new(type="ShaderNodeTexNoise")

    group.links.new(mapping_node.outputs["Vector"], peeling_noise_node.inputs["Vector"])
    group.links.new(input_node.outputs["Scale"], peeling_noise_node.inputs["Scale"])
    group.links.new(input_node.outputs["Detail"], peeling_noise_node.inputs["Detail"])
    group.links.new(input_node.outputs["Distortion"], peeling_noise_node.inputs["Distortion"])

    peeling_threshold_node = create_parametric_color_ramp_node(group)
    peeling_threshold_node.inputs["Color1"].default_value = (0.0, 0.0, 0.0, 1.0)
    peeling_threshold_node.inputs["Color2"].default_value = (1.0, 1.0, 1.0, 1.0)

    # Base color

    epsilon_subtract_node = group.nodes.new(type="ShaderNodeMath")
    epsilon_subtract_node.operation = "SUBTRACT"
    epsilon_subtract_node.inputs[1].default_value = 0.001

    group.links.new(input_node.outputs["Threshold"], epsilon_subtract_node.inputs[0])

    group.links.new(peeling_noise_node.outputs["Fac"], peeling_threshold_node.inputs["Fac"])
    group.links.new(epsilon_subtract_node.outputs["Value"], peeling_threshold_node.inputs["Pos1"])
    group.links.new(input_node.outputs["Threshold"], peeling_threshold_node.inputs["Pos2"])

    color_mix_node = group.nodes.new(type="ShaderNodeMixRGB")
    group.links.new(peeling_threshold_node.outputs["Color"], color_mix_node.inputs["Fac"])
    group.links.new(input_node.outputs["Metal Color"], color_mix_node.inputs[1])
    group.links.new(input_node.outputs["Paint Color"], color_mix_node.inputs[2])

    # Ambient occulusion

    epsilon_add_node = group.nodes.new(type="ShaderNodeMath")
    epsilon_add_node.operation = "ADD"
    epsilon_add_node.inputs[1].default_value = 0.010

    group.links.new(input_node.outputs["Threshold"], epsilon_add_node.inputs[0])

    fallout_subtract_node = group.nodes.new(type="ShaderNodeMath")
    fallout_subtract_node.operation = "SUBTRACT"
    fallout_subtract_node.inputs[1].default_value = 0.060

    group.links.new(input_node.outputs["Threshold"], fallout_subtract_node.inputs[0])

    ao_node = create_tri_parametric_color_ramp_node(group)
    ao_node.inputs["Color1"].default_value = (1.0, 1.0, 1.0, 1.0)
    ao_node.inputs["Color2"].default_value = (0.0, 0.0, 0.0, 1.0)
    ao_node.inputs["Color3"].default_value = (1.0, 1.0, 1.0, 1.0)

    group.links.new(peeling_noise_node.outputs["Fac"], ao_node.inputs["Fac"])
    group.links.new(fallout_subtract_node.outputs["Value"], ao_node.inputs["Pos1"])
    group.links.new(input_node.outputs["Threshold"], ao_node.inputs["Pos2"])
    group.links.new(epsilon_add_node.outputs["Value"], ao_node.inputs["Pos3"])

    ao_mix_node = group.nodes.new(type="ShaderNodeMixRGB")
    ao_mix_node.blend_type = "MULTIPLY"
    ao_mix_node.inputs["Fac"].default_value = 1.0

    group.links.new(color_mix_node.outputs["Color"], ao_mix_node.inputs[1])
    group.links.new(ao_node.outputs["Color"], ao_mix_node.inputs[2])

    # Metallic

    metallic_node = group.nodes.new(type="ShaderNodeMixRGB")
    metallic_node.inputs["Color1"].default_value = (1.0, 1.0, 1.0, 1.0)
    metallic_node.inputs["Color2"].default_value = (0.0, 0.0, 0.0, 1.0)

    group.links.new(peeling_threshold_node.outputs["Color"], metallic_node.inputs["Fac"])

    # Roughness

    roughness_node = group.nodes.new(type="ShaderNodeMixRGB")
    roughness_node.inputs["Color1"].default_value = (0.50, 0.50, 0.50, 1.0)
    roughness_node.inputs["Color2"].default_value = (0.05, 0.05, 0.05, 1.0)

    group.links.new(peeling_threshold_node.outputs["Color"], roughness_node.inputs["Fac"])

    # Bump

    height_node = create_tri_parametric_color_ramp_node(group)
    height_node.inputs["Color1"].default_value = (0.0, 0.0, 0.0, 1.0)
    height_node.inputs["Color2"].default_value = (1.0, 1.0, 1.0, 1.0)
    height_node.inputs["Color3"].default_value = (0.5, 0.5, 0.5, 1.0)

    height_peak_add_node = group.nodes.new(type="ShaderNodeMath")
    height_peak_add_node.operation = "ADD"
    height_peak_add_node.inputs[1].default_value = 0.005

    height_tail_add_node = group.nodes.new(type="ShaderNodeMath")
    height_tail_add_node.operation = "ADD"
    height_tail_add_node.inputs[1].default_value = 0.025

    group.links.new(input_node.outputs["Threshold"], height_peak_add_node.inputs[0])
    group.links.new(input_node.outputs["Threshold"], height_tail_add_node.inputs[0])
    group.links.new(peeling_noise_node.outputs["Fac"], height_node.inputs["Fac"])
    group.links.new(input_node.outputs["Threshold"], height_node.inputs["Pos1"])
    group.links.new(height_peak_add_node.outputs["Value"], height_node.inputs["Pos2"])
    group.links.new(height_tail_add_node.outputs["Value"], height_node.inputs["Pos3"])

    bump_node = group.nodes.new(type="ShaderNodeBump")
    group.links.new(height_node.outputs["Color"], bump_node.inputs["Height"])

    # Output

    output_node = group.nodes.new("NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Color")
    group.outputs.new("NodeSocketColor", "Metallic")
    group.outputs.new("NodeSocketColor", "Roughness")
    group.outputs.new("NodeSocketVectorDirection", "Bump")

    group.links.new(ao_mix_node.outputs["Color"], output_node.inputs["Color"])
    group.links.new(metallic_node.outputs["Color"], output_node.inputs["Metallic"])
    group.links.new(roughness_node.outputs["Color"], output_node.inputs["Roughness"])
    group.links.new(bump_node.outputs["Normal"], output_node.inputs["Bump"])

    arrange_nodes(group)

    return group


def create_peeling_paint_metal_node_group(node_tree: bpy.types.NodeTree) -> bpy.types.Node:
    peeling_paint_metal_node_group: bpy.types.NodeGroup

    if "Peeling Paint Metal" in bpy.data.node_groups:
        peeling_paint_metal_node_group = bpy.data.node_groups["Peeling Paint Metal"]
    else:
        peeling_paint_metal_node_group = add_peeling_paint_metal_node_group()

    node = node_tree.nodes.new(type='ShaderNodeGroup')
    node.name = "Peeling Paint Metal"
    node.node_tree = peeling_paint_metal_node_group

    return node


def build_peeling_paint_metal_nodes(node_tree: bpy.types.NodeTree) -> None:
    output_node = node_tree.nodes.new(type='ShaderNodeOutputMaterial')
    principled_node = node_tree.nodes.new(type='ShaderNodeBsdfPrincipled')
    peeling_paint_metal_node = create_peeling_paint_metal_node_group(node_tree)

    node_tree.links.new(peeling_paint_metal_node.outputs['Color'], principled_node.inputs['Base Color'])
    node_tree.links.new(peeling_paint_metal_node.outputs['Metallic'], principled_node.inputs['Metallic'])
    node_tree.links.new(peeling_paint_metal_node.outputs['Roughness'], principled_node.inputs['Roughness'])
    node_tree.links.new(peeling_paint_metal_node.outputs['Bump'], principled_node.inputs['Normal'])
    node_tree.links.new(principled_node.outputs['BSDF'], output_node.inputs['Surface'])

    arrange_nodes(node_tree)
