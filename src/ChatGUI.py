import socket
import threading
import tkinter as tk
from tkinter import scrolledtext, simpledialog, messagebox

# --- CẤU HÌNH ---
SERVER_IP = '192.168.191.38' 
SERVER_PORT = 8080

class ChatClientGUI:
    def __init__(self, master):
        self.master = master
        master.title("Haskell STM Chat")
        master.geometry("400x500")

        # 1. Khu vực hiển thị tin nhắn
        self.chat_area = scrolledtext.ScrolledText(master, state='disabled', wrap='word')
        self.chat_area.pack(padx=10, pady=10, fill=tk.BOTH, expand=True)
        self.chat_area.tag_config('me', foreground='blue')
        self.chat_area.tag_config('server', foreground='green')
        self.chat_area.tag_config('error', foreground='red')

        # 2. Khu vực nhập tin nhắn
        self.input_frame = tk.Frame(master)
        self.input_frame.pack(padx=10, pady=5, fill=tk.X)

        self.msg_entry = tk.Entry(self.input_frame)
        self.msg_entry.pack(side=tk.LEFT, fill=tk.X, expand=True)
        self.msg_entry.bind("<Return>", self.send_msg)

        self.send_btn = tk.Button(self.input_frame, text="Gửi", command=self.send_msg)
        self.send_btn.pack(side=tk.RIGHT, padx=(5, 0))

        # 3. Kết nối
        self.sock = None
        self.connect_to_server()

    def connect_to_server(self):
        try:
            # Hỏi tên và phòng
            self.username = simpledialog.askstring("Login", "Nhập tên của bạn:", parent=self.master)
            if not self.username: self.master.destroy(); return
            
            self.room = simpledialog.askstring("Room", "Nhập tên phòng (VD: #demo):", parent=self.master)
            if not self.room: self.room = "#general"

            # Kết nối Socket
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((SERVER_IP, SERVER_PORT))

            # Gửi lệnh đăng nhập
            self.sock.sendall(f"LOGIN {self.username}\n".encode('utf-8'))
            self.sock.sendall(f"JOIN {self.room}\n".encode('utf-8'))

            self.append_log(f"Đã kết nối đến {SERVER_IP} vào phòng {self.room}", 'server')

            # Tạo luồng nhận tin nhắn
            threading.Thread(target=self.receive_loop, daemon=True).start()

        except Exception as e:
            messagebox.showerror("Lỗi kết nối", str(e))
            self.master.destroy()

    def receive_loop(self):
        while True:
            try:
                data = self.sock.recv(1024)
                if not data: break
                msg = data.decode('utf-8').strip()
                # Cập nhật giao diện từ luồng khác
                self.master.after(0, self.append_log, msg)
            except:
                break

    def send_msg(self, event=None):
        msg = self.msg_entry.get()
        if msg:
            if msg == 'QUIT':
                self.master.destroy()
                return
            
            # Gửi lệnh MSG theo đúng chuẩn Haskell Server
            # Cú pháp: MSG #room noi_dung
            full_cmd = f"MSG {self.room} {msg}\n"
            try:
                self.sock.sendall(full_cmd.encode('utf-8'))
                self.msg_entry.delete(0, tk.END)
                # Tự hiện tin nhắn của mình lên (vì server Haskell broadcast cho cả người gửi)
                # self.append_log(f"Me: {msg}", 'me') 
            except:
                self.append_log("Lỗi gửi tin!", 'error')

    def append_log(self, msg, tag=None):
        self.chat_area.config(state='normal')
        self.chat_area.insert(tk.END, msg + '\n', tag)
        self.chat_area.see(tk.END)
        self.chat_area.config(state='disabled')

# Chạy chương trình
if __name__ == "__main__":
    root = tk.Tk()
    gui = ChatClientGUI(root)
    root.mainloop()