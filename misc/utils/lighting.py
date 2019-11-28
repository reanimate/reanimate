import bpy
from typing import Optional, Tuple


def create_area_light(location: Tuple[float, float, float] = (0.0, 0.0, 5.0),
                      rotation: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                      size: float = 5.0,
                      color: Tuple[float, float, float, float] = (1.00, 0.90, 0.80, 1.00),
                      strength: float = 1000.0,
                      name: Optional[str] = None) -> bpy.types.Object:
    if bpy.app.version >= (2, 80, 0):
        bpy.ops.object.light_add(type='AREA', location=location, rotation=rotation)
    else:
        bpy.ops.object.lamp_add(type='AREA', location=location, rotation=rotation)

    if name is not None:
        bpy.context.object.name = name

    light = bpy.context.object.data
    light.size = size
    light.use_nodes = True
    light.node_tree.nodes["Emission"].inputs["Color"].default_value = color
    if bpy.app.version >= (2, 80, 0):
        light.energy = strength
    else:
        light.node_tree.nodes["Emission"].inputs["Strength"].default_value = strength

    return bpy.context.object


def create_sun_light(location: Tuple[float, float, float] = (0.0, 0.0, 5.0),
                     rotation: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                     name: Optional[str] = None) -> bpy.types.Object:
    if bpy.app.version >= (2, 80, 0):
        bpy.ops.object.light_add(type='SUN', location=location, rotation=rotation)
    else:
        bpy.ops.object.lamp_add(type='SUN', location=location, rotation=rotation)

    if name is not None:
        bpy.context.object.name = name

    return bpy.context.object
