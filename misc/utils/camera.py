import bpy
from typing import Tuple


def create_camera(location: Tuple[float, float, float]) -> bpy.types.Object:
    bpy.ops.object.camera_add(location=location)

    return bpy.context.object


def set_camera_params(camera: bpy.types.Camera,
                      focus_target_object: bpy.types.Object,
                      lens: float = 85.0,
                      fstop: float = 1.4) -> None:
    # Simulate Sony's FE 85mm F1.4 GM
    camera.sensor_fit = 'HORIZONTAL'
    camera.sensor_width = 36.0
    camera.sensor_height = 24.0
    camera.lens = lens

    if bpy.app.version >= (2, 80, 0):
        camera.dof.use_dof = True
        camera.dof.focus_object = focus_target_object
        camera.dof.aperture_fstop = fstop
        camera.dof.aperture_blades = 11
    else:
        camera.dof_object = focus_target_object
        camera.cycles.aperture_type = 'FSTOP'
        camera.cycles.aperture_fstop = fstop
        camera.cycles.aperture_blades = 11
