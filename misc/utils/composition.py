import bpy
from utils.node import set_socket_value_range, clean_nodes, arrange_nodes


def add_split_tone_node_group():
    group = bpy.data.node_groups.new(type="CompositorNodeTree", name="SplitToneSub")

    input_node = group.nodes.new("NodeGroupInput")
    group.inputs.new("NodeSocketColor", "Image")
    group.inputs.new("NodeSocketFloat", "Hue")
    group.inputs.new("NodeSocketFloat", "Saturation")

    solid_node = group.nodes.new(type="CompositorNodeCombHSVA")
    solid_node.inputs["S"].default_value = 1.0
    solid_node.inputs["V"].default_value = 1.0
    solid_node.inputs["A"].default_value = 1.0

    input_sep_node = group.nodes.new(type="CompositorNodeSepHSVA")

    overlay_node = group.nodes.new(type="CompositorNodeMixRGB")
    overlay_node.blend_type = 'OVERLAY'

    overlay_sep_node = group.nodes.new(type="CompositorNodeSepHSVA")

    comb_node = group.nodes.new(type="CompositorNodeCombHSVA")

    output_node = group.nodes.new("NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Image")

    group.links.new(input_node.outputs["Hue"], solid_node.inputs["H"])
    group.links.new(input_node.outputs["Saturation"], overlay_node.inputs["Fac"])
    group.links.new(input_node.outputs["Image"], overlay_node.inputs[1])
    group.links.new(solid_node.outputs["Image"], overlay_node.inputs[2])
    group.links.new(overlay_node.outputs["Image"], overlay_sep_node.inputs["Image"])
    group.links.new(input_node.outputs["Image"], input_sep_node.inputs["Image"])
    group.links.new(overlay_sep_node.outputs["H"], comb_node.inputs["H"])
    group.links.new(overlay_sep_node.outputs["S"], comb_node.inputs["S"])
    group.links.new(input_sep_node.outputs["V"], comb_node.inputs["V"])
    group.links.new(input_sep_node.outputs["A"], comb_node.inputs["A"])
    group.links.new(comb_node.outputs["Image"], output_node.inputs["Image"])

    arrange_nodes(group)

    # --------------------------------------------------------------------------

    group = bpy.data.node_groups.new(type="CompositorNodeTree", name="SplitTone")

    input_node = group.nodes.new("NodeGroupInput")

    group.inputs.new("NodeSocketColor", "Image")
    group.inputs.new("NodeSocketFloat", "HighlightsHue")
    group.inputs.new("NodeSocketFloat", "HighlightsSaturation")
    group.inputs.new("NodeSocketFloat", "ShadowsHue")
    group.inputs.new("NodeSocketFloat", "ShadowsSaturation")
    group.inputs.new("NodeSocketFloatFactor", "Balance")

    set_socket_value_range(group.inputs["HighlightsHue"])
    set_socket_value_range(group.inputs["HighlightsSaturation"])
    set_socket_value_range(group.inputs["ShadowsHue"])
    set_socket_value_range(group.inputs["ShadowsSaturation"])
    set_socket_value_range(group.inputs["Balance"], default_value=0.5)

    input_sep_node = group.nodes.new(type="CompositorNodeSepHSVA")

    subtract_node = group.nodes.new(type="CompositorNodeMath")
    subtract_node.inputs[0].default_value = 1.0
    subtract_node.operation = 'SUBTRACT'
    subtract_node.use_clamp = True

    multiply_node = group.nodes.new(type="CompositorNodeMath")
    multiply_node.inputs[1].default_value = 2.0
    multiply_node.operation = 'MULTIPLY'
    multiply_node.use_clamp = False

    power_node = group.nodes.new(type="CompositorNodeMath")
    power_node.operation = 'POWER'
    power_node.use_clamp = True

    shadows_node = group.nodes.new(type='CompositorNodeGroup')
    shadows_node.name = "Shadows"
    shadows_node.node_tree = bpy.data.node_groups["SplitToneSub"]

    highlights_node = group.nodes.new(type='CompositorNodeGroup')
    highlights_node.name = "Highlights"
    highlights_node.node_tree = bpy.data.node_groups["SplitToneSub"]

    comb_node = group.nodes.new(type="CompositorNodeMixRGB")
    comb_node.use_clamp = False

    output_node = group.nodes.new("NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Image")

    group.links.new(input_node.outputs["Image"], input_sep_node.inputs["Image"])
    group.links.new(input_node.outputs["Image"], shadows_node.inputs["Image"])
    group.links.new(input_node.outputs["ShadowsHue"], shadows_node.inputs["Hue"])
    group.links.new(input_node.outputs["ShadowsSaturation"], shadows_node.inputs["Saturation"])
    group.links.new(input_node.outputs["Image"], highlights_node.inputs["Image"])
    group.links.new(input_node.outputs["HighlightsHue"], highlights_node.inputs["Hue"])
    group.links.new(input_node.outputs["HighlightsSaturation"], highlights_node.inputs["Saturation"])
    group.links.new(input_node.outputs["Balance"], subtract_node.inputs[1])
    group.links.new(subtract_node.outputs["Value"], multiply_node.inputs[0])
    group.links.new(input_sep_node.outputs["V"], power_node.inputs[0])
    group.links.new(multiply_node.outputs["Value"], power_node.inputs[1])
    group.links.new(power_node.outputs["Value"], comb_node.inputs["Fac"])
    group.links.new(shadows_node.outputs["Image"], comb_node.inputs[1])
    group.links.new(highlights_node.outputs["Image"], comb_node.inputs[2])
    group.links.new(comb_node.outputs["Image"], output_node.inputs["Image"])

    arrange_nodes(group)

    return group


def add_vignette_node_group():
    group = bpy.data.node_groups.new(type="CompositorNodeTree", name="Vignette")

    input_node = group.nodes.new("NodeGroupInput")
    group.inputs.new("NodeSocketColor", "Image")
    group.inputs.new("NodeSocketFloat", "Amount")
    group.inputs["Amount"].default_value = 0.2
    group.inputs["Amount"].min_value = 0.0
    group.inputs["Amount"].max_value = 1.0

    lens_distortion_node = group.nodes.new(type="CompositorNodeLensdist")
    lens_distortion_node.inputs["Distort"].default_value = 1.000

    separate_rgba_node = group.nodes.new(type="CompositorNodeSepRGBA")

    blur_node = group.nodes.new(type="CompositorNodeBlur")
    blur_node.filter_type = 'GAUSS'
    blur_node.size_x = 300
    blur_node.size_y = 300
    blur_node.use_extended_bounds = True

    mix_node = group.nodes.new(type="CompositorNodeMixRGB")
    mix_node.blend_type = 'MULTIPLY'

    output_node = group.nodes.new("NodeGroupOutput")
    group.outputs.new("NodeSocketColor", "Image")

    group.links.new(input_node.outputs["Amount"], mix_node.inputs["Fac"])
    group.links.new(input_node.outputs["Image"], mix_node.inputs[1])
    group.links.new(input_node.outputs["Image"], lens_distortion_node.inputs["Image"])
    group.links.new(lens_distortion_node.outputs["Image"], separate_rgba_node.inputs["Image"])
    group.links.new(separate_rgba_node.outputs["A"], blur_node.inputs["Image"])
    group.links.new(blur_node.outputs["Image"], mix_node.inputs[2])
    group.links.new(mix_node.outputs["Image"], output_node.inputs["Image"])

    arrange_nodes(group)

    return group


def create_split_tone_node(node_tree):
    split_tone_node_group = add_split_tone_node_group()

    node = node_tree.nodes.new(type='CompositorNodeGroup')
    node.name = "SplitTone"
    node.node_tree = split_tone_node_group

    return node


def create_vignette_node(node_tree):
    vignette_node_group = add_vignette_node_group()

    node = node_tree.nodes.new(type='CompositorNodeGroup')
    node.name = "Vignette"
    node.node_tree = vignette_node_group

    return node


def build_scene_composition(scene, vignette=0.20, dispersion=0.050, gain=1.10, saturation=1.10):
    scene.use_nodes = True
    clean_nodes(scene.node_tree.nodes)

    render_layer_node = scene.node_tree.nodes.new(type="CompositorNodeRLayers")

    vignette_node = create_vignette_node(scene.node_tree)
    vignette_node.inputs["Amount"].default_value = vignette

    lens_distortion_node = scene.node_tree.nodes.new(type="CompositorNodeLensdist")
    lens_distortion_node.inputs["Distort"].default_value = -dispersion * 0.40
    lens_distortion_node.inputs["Dispersion"].default_value = dispersion

    color_correction_node = scene.node_tree.nodes.new(type="CompositorNodeColorCorrection")
    color_correction_node.master_saturation = saturation
    color_correction_node.master_gain = gain

    split_tone_node = create_split_tone_node(scene.node_tree)

    glare_node = scene.node_tree.nodes.new(type="CompositorNodeGlare")
    glare_node.glare_type = 'FOG_GLOW'
    glare_node.quality = 'HIGH'

    composite_node = scene.node_tree.nodes.new(type="CompositorNodeComposite")

    scene.node_tree.links.new(render_layer_node.outputs['Image'], vignette_node.inputs['Image'])
    scene.node_tree.links.new(vignette_node.outputs['Image'], lens_distortion_node.inputs['Image'])
    scene.node_tree.links.new(lens_distortion_node.outputs['Image'], color_correction_node.inputs['Image'])
    scene.node_tree.links.new(color_correction_node.outputs['Image'], split_tone_node.inputs['Image'])
    scene.node_tree.links.new(split_tone_node.outputs['Image'], glare_node.inputs['Image'])
    scene.node_tree.links.new(glare_node.outputs['Image'], composite_node.inputs['Image'])

    arrange_nodes(scene.node_tree)
