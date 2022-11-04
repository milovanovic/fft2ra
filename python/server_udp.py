import sys
import socket
import numpy as np
from pyqtgraph.Qt import QtCore, QtGui, QtWidgets
import pyqtgraph as pg
#import pyqtgraph.ptime as ptime
import sys
import time as systime
from PyQt5.QtCore import QObject, QThread, pyqtSignal

def bytes_to_np_array(data, dtype, byteorder):
    return np.frombuffer(data, dtype=dtype.newbyteorder(byteorder)).astype(int)

def complexmodgen(max_mag, np_real_imag_dtype='int16', endian='big'):
    def complexmod(data):
        if type(data) is not bytes:
            raise ValueError(f"data is of type {type(c)}, not {bytes}")
        units = bytes_to_np_array(data, np.dtype(np_real_imag_dtype), '>' if endian == 'big' else '<')
        pairs = units.reshape(-1, 2)
        sqrmodulos = np.sum(np.square(pairs), axis=1)
        scalars = (sqrmodulos / max_mag).clip(0, 1)
        return scalars
    return complexmod

def realmodgen(max_mag, np_real_imag_dtype='uint16', endian='big'):
    def realmod(data):
        if type(data) is not bytes:
            raise ValueError(f"data is of type {type(c)}, not {bytes}")
        units = bytes_to_np_array(data, np.dtype(np_real_imag_dtype), '>' if endian == 'big' else '<')
        scalars = (units / max_mag).clip(0, 1)
        #scalars = units #(units / max_mag)
        return scalars
    return realmod

#UDP_IP = "127.0.0.1"
UDP_IP = "192.168.33.30"
UDP_PORT = 4098
#IMAGE_SIZE = [256, 8]
IMAGE_SIZE = [256, 32]
PACKET_SIZE = 1026
PACKET_DISCARD_BYTES = 2
UNIT_SIZE = 2 # 4 # bytes per data unit
#UNIT_INTENSITY_FUNC = complexmodgen(2*16384*16384, 'int16', 'big')
#UNIT_INTENSITY_FUNC = complexmodgen(2*2000, 'int16', 'big') # 2000 15000
UNIT_INTENSITY_FUNC = realmodgen(200, 'uint16', 'big') # 2*100
PROTOCOL = "UDP"
#PROTOCOL = "TCP"
PACKET_BUFFER_SIZE = 1026
LOGSCALE = True

def make_gradient(height, width):
    vals = []
    for row in range(height):
        for col in range(width):
            vals.append(row / (height-1) * col / (width - 1))
    levels = np.round(np.array(vals) * 255).astype(np.uint8)
    imat = levels.reshape([height, width])
    return imat
    
class Receiver(object):
    def __init__(self, udp_ip, udp_port, image_size, packet_size, packet_discard_bytes, unit_size, unit_intensity_func, protocol, logscale, packet_buffer_size=None):
        self.effective_packet_size = packet_size - packet_discard_bytes
        if len(image_size) != 2:
            raise ValueError("image_size must be of length 2")
        if self.effective_packet_size % unit_size != 0:
            raise ValueError(f"Effective packet size ({self.effective_packet_size}) is not divisible by unit size ({unit_size})")
        units_per_packet = self.effective_packet_size // unit_size
        units_per_image = image_size[0] * image_size[1]
        if units_per_image % units_per_packet != 0:
            raise ValueError(f"Number of data units composing an image ({units_per_image}) is not divisible by the number of data units per packet ({units_per_packet})")
        
        if packet_buffer_size is None:
            packet_buffer_size = 2 * self.effective_packet_size
        self.packet_buffer_size = packet_buffer_size
        
        if protocol != "UDP" and protocol != "TCP":
            raise ValueError("Protocol must be either UDP or TCP")
        self.protocol = protocol
        if self.protocol == "UDP":
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self.sock.bind((udp_ip, udp_port))
        elif self.protocol == "TCP":
            self.sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_STREAM)
            self.sock.bind((udp_ip, udp_port))
            self.sock.listen()
            self.conn, self.addr = self.sock.accept()
        
        self.image_size = image_size
        self.packet_size = packet_size
        self.packet_discard_bytes = packet_discard_bytes
        self.unit_size = unit_size
        self.unit_intensity_func = unit_intensity_func
        self.packet_buffer_size = packet_buffer_size
        self.image_num_packets = units_per_image // units_per_packet
        print(f"{self.__class__.__name__}: Expecting {self.image_num_packets} packets with {self.packet_size} bytes ({self.effective_packet_size} effective bytes) to form a single image.")
        self.logscale = logscale
    
    def recv_packet_data(self):
        if self.protocol == "UDP":
            data = self.sock.recv(self.packet_buffer_size)
        elif self.protocol == "TCP":
            data = self.conn.recv(self.packet_buffer_size)
        if len(data) != self.packet_size:
            raise ValueError(f"Invalid packet byte length {len(data)}, {self.packet_size} bytes expected")
        return data[self.packet_discard_bytes:]
    
    def safe_recv_packet_data(self):
        try:
            data = self.recv_packet_data()
        except ValueError as e:
            print(f"Ignoring invalid packet due to error \"{str(e)}\" and assuming a zeroed-out block of data.")
            data = bytes(self.effective_packet_size)
        return data
    
    def recv_image_data(self):
        scalars = np.concatenate([ self.unit_intensity_func(self.safe_recv_packet_data()) for _ in range(self.image_num_packets) ])
        if self.logscale == False:
            levels = np.round(scalars * 255.0).clip(0, 255).astype(np.uint8)
        else:
            logs = np.log10(scalars * 20.0 + 0.000000001)
            #logs = np.log10(scalars + 0.000000001)
            logs[logs < 0] = 0
            levels = np.round(logs*200.0).clip(0, 255).astype(np.uint8)
        return levels
    
    def next_image(self):
        #return self.recv_image_data().reshape(self.image_size)
        return np.fft.fftshift(self.recv_image_data().reshape(self.image_size), 1)


class Worker(QObject):
    update = pyqtSignal()
    def __init__(self, receiver):
        global IMAGE_SIZE
        super().__init__()
        self.r = receiver

    def run(self):
        while True:
            self.data = r.next_image()
            self.update.emit()

class MyOpenGLWindow( QtGui.QOpenGLWindow ):
    def __init__( self, image_size, render_angle=90, render_num_points=1024 ):
        super().__init__()
        if render_num_points < 2:
            raise ValueError("render_num_points must be at least 2")
        self.angle = render_angle
        self.num_points = render_num_points
        self.profile = QtGui.QOpenGLVersionProfile()
        self.profile.setVersion( 2, 0 )
        
        h, w = image_size
        self.imat = make_gradient(h, w)
        self.image = QtGui.QImage(self.imat.data, w, h, w, QtGui.QImage.Format_Grayscale8)#.mirrored()
#        self.image = QtGui.QImage("/home/konda/Screenshot from 2022-09-19 09-19-13.png").mirrored()

    def rads(self):
        return np.radians(self.angle)
    
    def intertwine(self, arrs):
        assert len(arrs) > 0
        arr_len = len(arrs[0])
        dimens = len(arrs)
        assert all(len(arr) == arr_len for arr in arrs)
        c = np.empty(dimens * arr_len)
        for i in range(len(arrs)):
            c[i::dimens] = arrs[i]
        return c
        #b = np.fft.fftshift(c)
        #return b

    def initializeGL( self ):
        self.gl = self.context().versionFunctions( self.profile )
        
        self.vao_offscreen = QtGui.QOpenGLVertexArrayObject( self )
        self.vao_offscreen.create()
        self.vao = QtGui.QOpenGLVertexArrayObject( self )
        self.vao.create()
        self.texture = QtGui.QOpenGLTexture( self.image, QtGui.QOpenGLTexture.DontGenerateMipMaps )
        self.texture.setWrapMode(QtGui.QOpenGLTexture.ClampToEdge)
        self.texture.setMagnificationFilter(QtGui.QOpenGLTexture.Linear)

        self.program = QtGui.QOpenGLShaderProgram( self )
        self.program.addShaderFromSourceFile( QtGui.QOpenGLShader.Vertex, 'simple_texture.vs' )
        self.program.addShaderFromSourceFile( QtGui.QOpenGLShader.Fragment, 'simple_texture.fs' )
        self.program.link()

        self.program.bind()
        self.matrix = QtGui.QMatrix4x4()
        self.matrix.ortho( 0, 1, 0, 1, 0, 1 )
        self.program.setUniformValue( "mvp", self.matrix )
        self.program.release()

        # screen render
        angles = np.arange(self.num_points) * self.rads() / (self.num_points-1)
        angles_centered = angles + (np.pi - self.rads()) / 2
#        vx = self.intertwine([np.array([0.5] * self.num_points), np.cos(angles_centered) / 2 + 0.5])
        vx = self.intertwine([0.2 * np.cos(angles_centered) / 2 + 0.5, np.cos(angles_centered) / 2 + 0.5])
        vx /= vx.max() - vx.min()
        vx -= vx.min()
#        vy = self.intertwine([np.array([0.5] * self.num_points), np.sin(angles_centered) / 2 + 0.5])
        vy = self.intertwine([0.2 * np.sin(angles_centered) / 2 + 0.5, np.sin(angles_centered) / 2 + 0.5])
        vy /= vy.max() - vy.min()
        vy -= vy.min()
        vz = np.zeros(self.num_points * 2)
        tx = np.repeat(np.linspace(1, 0, self.num_points), 2)
        ty = np.array([0.0, 1.0] * self.num_points)
        self.vertices = self.intertwine([vx, vy, vz])
        self.tex = self.intertwine([tx, ty])
        
        self.vao.bind()
        #self.vertices = [ 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0 ]
        self.vbo_vertices = self.setVertexBuffer( self.vertices, 3, self.program, "position" )
        #self.tex = [ 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0 ]
        self.vbo_tex = self.setVertexBuffer( self.tex, 2, self.program, "texCoord" )
        self.vao.release()
        
        self.program.bind()

        self.texture.bind()
        self.vao.bind()
#        self.gl.glClearColor(0.0, 0.125, 0.125, 0.0)
#        self.update()
    
    def resizeGL( self, w, h ):
        self.gl.glClear(self.gl.GL_COLOR_BUFFER_BIT)

    def paintGL( self ):
        #self.gl.glViewport( 0, 0, self.image.width(), self.image.height() )

#        self.program.bind()

#        self.texture.bind()
#        self.vao.bind()
        self.gl.glClear(self.gl.GL_COLOR_BUFFER_BIT)
        self.gl.glDrawArrays( self.gl.GL_TRIANGLE_STRIP, 0, self.num_points * 2 )
#        self.vao.release()
#        self.texture.release()

#        self.program.release()
#        self.texture.setData(QtGui.QOpenGLTexture.Luminance, QtGui.QOpenGLTexture.UInt8, np.random.randint(0,255, [256,8]).astype(np.uint8))

    def setVertexBuffer( self, data_array, dim_vertex, program, shader_str ):
        vbo = QtGui.QOpenGLBuffer( QtGui.QOpenGLBuffer.VertexBuffer )
        vbo.create()
        vbo.bind()

        vertices = np.array( data_array, np.float32 )
        vbo.allocate( vertices, vertices.shape[0] * vertices.itemsize )

        attr_loc = program.attributeLocation( shader_str )
        program.enableAttributeArray( attr_loc )
        program.setAttributeBuffer( attr_loc, self.gl.GL_FLOAT, 0, dim_vertex )
        vbo.release()

        return vbo

if __name__ == '__main__':
    pg.setConfigOption('imageAxisOrder', 'row-major')
    app = QtWidgets.QApplication( sys.argv )

    window = MyOpenGLWindow(IMAGE_SIZE)
    window.resize( 768, 512 )
    window.show()
    
    #updateTime = ptime.time()
    updateTime = systime.time()
    
    r = Receiver(UDP_IP, UDP_PORT, IMAGE_SIZE, PACKET_SIZE, PACKET_DISCARD_BYTES, UNIT_SIZE, UNIT_INTENSITY_FUNC, PROTOCOL, LOGSCALE, PACKET_BUFFER_SIZE)
    t = QThread()
    w = Worker(r)
    w.moveToThread(t)
    t.started.connect(w.run)
    
    deltas = np.array([])

    def updateData():
        global window
        if not hasattr(window, 'texture'):
            return
        global updateTime, deltas, w

        ## Display the data
        window.texture.setData(QtGui.QOpenGLTexture.Luminance, QtGui.QOpenGLTexture.UInt8, w.data)
        window.update()
#        print("refresh")

#        QtCore.QTimer.singleShot(1, updateData)
        #now = ptime.time()
        now = systime.time()
        delta = now - updateTime
        updateTime = now
        
        deltas = np.append(deltas, [delta])[-1000:]
        if deltas.size == 1000:
            print(f"{np.mean(deltas)*1000:.2f} ms")
    
    w.update.connect(updateData)
    t.start()

    app.exec_()

#if __name__ == "__main__":
#    
#    pg.setConfigOption('imageAxisOrder', 'row-major')

#    app = QtGui.QApplication([])

#    ## Create window with GraphicsView widget
#    win = pg.GraphicsLayoutWidget()
#    win.show()  ## show widget alone in its own window
#    win.setWindowTitle('pyqtgraph example: ImageItem')
#    view = win.addViewBox()

#    ## lock the aspect ratio so pixels are always square
#    view.setAspectLocked(True)

#    ## Create image item
#    img = pg.ImageItem(border='w')
#    view.addItem(img)

#    ## Set initial view bounds
#    view.setRange(QtCore.QRectF(0, 0, 8, 256))

#    updateTime = ptime.time()
#    fps = 0
#    
#    r = Receiver("127.0.0.1", 5005)
#    t = QThread()
#    w = Worker(r)
#    w.moveToThread(t)
#    t.started.connect(w.run)
#    
#    deltas = np.array([])

#    def updateData():
#        global img, updateTime, fps, deltas

#        ## Display the data
#        img.setImage(w.data)
##        print("refresh")

##        QtCore.QTimer.singleShot(1, updateData)
#        now = ptime.time()
#        delta = now - updateTime
#        updateTime = now
#        
#        deltas = np.append(deltas, [delta])[-1000:]
#        if deltas.size == 1000:
#            print(f"{np.mean(deltas)*1000:.2f} ms")
#    
#    w.update.connect(updateData)
#    updateData()
#    t.start()

#    ## Start Qt event loop unless running in interactive mode.
#    if __name__ == '__main__':
#        import sys
#        if (sys.flags.interactive != 1) or not hasattr(QtCore, 'PYQT_VERSION'):
#            QtGui.QApplication.instance().exec_()

