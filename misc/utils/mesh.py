import bpy
import math
from typing import Tuple, Iterable, Optional, Sequence
from utils.modifier import add_subdivision_surface_modifier


def set_smooth_shading(mesh: bpy.types.Mesh) -> None:
    for polygon in mesh.polygons:
        polygon.use_smooth = True


def create_mesh_from_pydata(scene: bpy.types.Scene,
                            vertices: Iterable[Iterable[float]],
                            faces: Iterable[Iterable[int]],
                            mesh_name: str,
                            object_name: str,
                            use_smooth: bool = True) -> bpy.types.Object:
    # Add a new mesh and set vertices and faces
    # In this case, it does not require to set edges
    # After manipulating mesh data, update() needs to be called
    new_mesh: bpy.types.Mesh = bpy.data.meshes.new(mesh_name)
    new_mesh.from_pydata(vertices, [], faces)
    new_mesh.update()
    if use_smooth:
        set_smooth_shading(new_mesh)

    new_object: bpy.types.Object = bpy.data.objects.new(object_name, new_mesh)
    if bpy.app.version >= (2, 80, 0):
        scene.collection.objects.link(new_object)
    else:
        scene.objects.link(new_object)

    return new_object


def create_cached_mesh_from_alembic(file_path: str, name: str) -> bpy.types.Object:
    bpy.ops.wm.alembic_import(filepath=file_path, as_background_job=False)
    bpy.context.active_object.name = name

    return bpy.context.active_object


def create_plane(location: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                 rotation: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                 size: float = 2.0,
                 name: Optional[str] = None) -> bpy.types.Object:
    if bpy.app.version >= (2, 80, 0):
        bpy.ops.mesh.primitive_plane_add(size=size, location=location, rotation=rotation)
    else:
        bpy.ops.mesh.primitive_plane_add(radius=0.5 * size, calc_uvs=True, location=location, rotation=rotation)

    current_object = bpy.context.object

    if name is not None:
        current_object.name = name

    return current_object


def create_smooth_sphere(location: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                         radius: float = 1.0,
                         subdivision_level: int = 1,
                         name: Optional[str] = None) -> bpy.types.Object:
    if bpy.app.version >= (2, 80, 0):
        bpy.ops.mesh.primitive_uv_sphere_add(radius=radius, location=location, calc_uvs=True)
    else:
        bpy.ops.mesh.primitive_uv_sphere_add(size=radius, location=location, calc_uvs=True)

    current_object = bpy.context.object

    if name is not None:
        current_object.name = name

    set_smooth_shading(current_object.data)
    add_subdivision_surface_modifier(current_object, subdivision_level)

    return current_object


def create_smooth_monkey(location: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                         rotation: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                         subdivision_level: int = 2,
                         name: Optional[str] = None) -> bpy.types.Object:
    bpy.ops.mesh.primitive_monkey_add(location=location, rotation=rotation, calc_uvs=True)

    current_object = bpy.context.object

    if name is not None:
        current_object.name = name

    set_smooth_shading(current_object.data)
    add_subdivision_surface_modifier(current_object, subdivision_level)

    return current_object


def create_three_smooth_monkeys(names: Optional[Tuple[str, str, str]] = None
                                ) -> Tuple[bpy.types.Object, bpy.types.Object, bpy.types.Object]:
    if names is None:
        names = ("Suzanne Left", "Suzanne Center", "Suzanne Right")

    left = create_smooth_monkey(location=(-1.8, 0.0, 1.0), rotation=(0.0, 0.0, -math.pi * 60.0 / 180.0), name=names[0])
    center = create_smooth_monkey(location=(0.0, 0.0, 1.0), rotation=(0.0, 0.0, -math.pi * 60.0 / 180.0), name=names[1])
    right = create_smooth_monkey(location=(+1.8, 0.0, 1.0), rotation=(0.0, 0.0, -math.pi * 60.0 / 180.0), name=names[2])

    return left, center, right
