#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include <gnome.h>
#include "GUIDocument.h"
#include <sstream>
#include "BaseException.h"
#include <stdlib.h>

class CodeGenState {
   GtkWidget* dialog;
   GUIDocument *doc;
   GtkWidget *fileentry1;
   GtkWidget *combo_entry1;
   GtkWidget *combo_entry2;
   GtkWidget *checkbutton1;
   GtkWidget *checkbutton2;
   GtkWidget *checkbutton3;
public:
   CodeGenState(GUIDocument *_doc);
   ~CodeGenState() {gtk_widget_destroy(dialog);}
   void ok();
   void cancel() {}
};

void CodeGenState::ok()
{
   try {
      char *dirname=gnome_file_entry_get_full_path(GNOME_FILE_ENTRY(fileentry1),FALSE);
      if (!dirname || !strlen(dirname))
	 dirname = ".";

      //Should create directory??

      char *filename=(char*)gtk_entry_get_text(GTK_ENTRY(combo_entry2));
      if (!filename || !strlen(filename))
	 filename = "build_doc.cc";

      char *funcname = (char*)gtk_entry_get_text(GTK_ENTRY(combo_entry1));
      if (!funcname || !strlen(funcname))
	 funcname = "buildFunct";
 
      bool generateMain=false, copyDepend=false, createMakefile=false;
      bool compile = false;
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton1)))
      {
	 generateMain=true;
      }
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton2)))
      {
	 copyDepend=true;
      }
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton3)))
      {
	 createMakefile=true;
      }
      
      string fullname = string(dirname) + "/" + filename;
      //cerr << "fullname = " << fullname << endl;
      doc->less_print("Warning: automatic code generation is still experimental");
      ofstream out(fullname.c_str());
      set<string> nodeList = doc->genCode(out, funcname, copyDepend);

      doc->less_print(string("C++ code generated in '") + filename 
		      + "', build function name is '" + funcname + "'");

      if (generateMain)
      {
	 out << "\n\n";
	 if (!copyDepend)
	 {
	    out << "#include <path.h>\n\n";
	    out << "#include <iextensions.h>\n\n";
	 } else {
	    out << "#include \"iextensions.h\"\n\n";
	 }
	 out << "int main(int argc, char **argv)\n";
	 out << "{\n\n";
	 out << "   try {\n";
	 out << "      IExtensions::detect();\n";
	 if (!copyDepend)
	    out << "      scanDL();\n";
	 out << "      ParameterSet param;\n";
	 out << "      for (int arg = 1; arg<argc; arg++)\n";
	 out << "      {\n";
	 out << "         char arg_name[100];\n";
	 out << "         sprintf (arg_name, \"ARG%d\", arg);\n";
	 out << "         param.add(arg_name, ObjectRef (new String (argv[arg])));\n";
	 out << "      }\n";
	 out << "      Network *net = " << funcname << "(\"MAIN\", param);\n";
	 out << "      net->verifyConnect();\n";
	 out << "      net->initialize();\n";
	 out << "      net->getOutput(0,0);\n";
	 out << "\n";
	 

	 out << "   }\n";
	 out << "   catch (BaseException *e)\n";
	 out << "   {\n";
	 out << "      e->print();\n";
	 out << "      return 1;\n";
	 out << "   }\n";
	 out << "   catch (...) {\n";
	 out << "      cerr<<\"Unhandled exception in \"<<argv[0]<<endl;\n";
	 out << "      cerr<<\"Exiting\"<<endl;\n";
	 out << "      return 1;\n";
	 out << "   }\n";
	 out << "   return 0;\n";
	 out << "}\n";

      }

      if (copyDepend)
      {
	 set<string> fileList;
	 for (set<string>::iterator it=nodeList.begin();it!=nodeList.end();it++)
	 {
	    NodeInfo *info = UINodeRepository::Find(*it);
	    if (info)
	       fileList.insert(fileList.begin(), info->sourceFile);
	    //fileList.insert(fileList.begin(), UIDocument::externalDocInfo[*it]->sourceFile);
	 }
	 UINodeRepository::ProcessDependencies(fileList);

	 set<string>::iterator file = fileList.begin();
	 while (file != fileList.end())
	 {
	    string fullPath = UIDocument::findExternal(*file, "VFLOW_SOURCE", false);
	    if (fullPath == "")
	    {
	       cerr << "cannot find " << *file << endl;
	       file++;
	       continue;
	    }
	    
	    /*Copy the source files to the right directory, adding a license reminder*/
	    string outName = string(dirname) + "/" + *file;
	    FILE *fin = fopen (fullPath.c_str(), "r");
	    if (!fin)
	    {
	       cerr << "cannot open " << fullPath << endl;
	       file++;
	       continue;
	    }
	    FILE *fout = fopen (outName.c_str(), "w");
	    if (!fout)
	    {
	       fclose(fin);
	       cerr << "cannot create " << outName << endl;
	       file++;
	       continue;		  
	    }
	    fprintf (fout, "//This file is a copy used for static linking of Overflow applications. If it is\n");
	    fprintf (fout, "//part of the Overflow code base, then it is released under the LGPL license.\n");
	    fprintf (fout, "//For more information, see the COPYING file in the Overflow source directory.\n\n");
	    char buff[1024];
	    int bytesRead;
	    do {
	       bytesRead = fread(buff, 1, 1024, fin);
	       if (bytesRead)
		  fwrite(buff, 1, bytesRead, fout);
	    } while (bytesRead == 1024);
	    fclose(fin);
	    fclose(fout);
	    

	    //string cmd = string("\\cp -f ") + fullPath + " " + dirname;
	    //system(cmd.c_str());
	    //cerr << cmd << endl;
	    file++;
	 }
      }

      if (createMakefile)
      {
	 cerr << "Makefile generation not implemented yet" << endl;
      }
      /*if (compile)
      {
	 out.flush();
	 string source_name = string(filename) + ".cc";
	 string cmd = string("g++ -O3 -o ") + filename + " " + source_name + " `flowdesigner-config config libflow --cflags --libs` `gnome-config xml --cflags --libs` -lrfftw -lfftw &";
	 cerr << cmd << endl;
	 system(cmd.c_str());
	 }*/

   } catch (BaseException *e)
   {
      ostringstream str;
      e->print(str);
      doc->less_print(str.str());
   }
}

void GUIDocument_codegen(GUIDocument *doc)
{
   new CodeGenState(doc);
}



static void codegen_ok(GtkButton *button, CodeGenState *gen)
{
   gen->ok();
   delete gen;
}

static void codegen_cancel(GtkButton *button, CodeGenState *gen)
{
   gen->cancel();
   delete gen;
}

CodeGenState::CodeGenState(GUIDocument *_doc)
   : doc(_doc)
{
   GtkWidget *window1;
   GtkWidget *vbox1;
   GtkWidget *label1;
   GtkWidget *hseparator1;
   GtkWidget *table1;
   GtkWidget *entry1;
   GtkWidget *entry2;
   GtkWidget *combo_entry3;
   GtkWidget *label2;
   GtkWidget *label3;
   GtkWidget *label4;
   GtkWidget *vbox2;
   GSList *vbox2_group = NULL;
   GtkWidget *hseparator2;
   GtkWidget *hbuttonbox1;
   GtkWidget *button1;
   GtkWidget *button2;


   window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
   gtk_window_set_title (GTK_WINDOW (window1), _("Code generation"));

   vbox1 = gtk_vbox_new (FALSE, 0);
   gtk_widget_ref (vbox1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "vbox1", vbox1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (vbox1);
   gtk_container_add (GTK_CONTAINER (window1), vbox1);

   label1 = gtk_label_new (_("Code generator"));
   gtk_widget_ref (label1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "label1", label1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label1);
   gtk_box_pack_start (GTK_BOX (vbox1), label1, FALSE, FALSE, 0);

   hseparator1 = gtk_hseparator_new ();
   gtk_widget_ref (hseparator1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "hseparator1", hseparator1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (hseparator1);
   gtk_box_pack_start (GTK_BOX (vbox1), hseparator1, TRUE, TRUE, 0);

   table1 = gtk_table_new (3, 2, FALSE);
   gtk_widget_ref (table1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "table1", table1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (table1);
   gtk_box_pack_start (GTK_BOX (vbox1), table1, TRUE, TRUE, 0);

   label2 = gtk_label_new (_("Build function name"));
   gtk_widget_ref (label2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "label2", label2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label2);
   gtk_table_attach (GTK_TABLE (table1), label2, 0, 1, 0, 1,
		     (GtkAttachOptions) (0),
		     (GtkAttachOptions) (0), 0, 0);

   label3 = gtk_label_new (_("Output file name"));
   gtk_widget_ref (label3);
   gtk_object_set_data_full (GTK_OBJECT (window1), "label3", label3,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label3);
   gtk_table_attach (GTK_TABLE (table1), label3, 0, 1, 1, 2,
		     (GtkAttachOptions) (0),
		     (GtkAttachOptions) (0), 0, 0);

   label4 = gtk_label_new (_("Directory"));
   gtk_widget_ref (label4);
   gtk_object_set_data_full (GTK_OBJECT (window1), "label4", label4,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (label4);
   gtk_table_attach (GTK_TABLE (table1), label4, 0, 1, 2, 3,
		     (GtkAttachOptions) (0),
		     (GtkAttachOptions) (0), 0, 0);

   entry1 = gnome_entry_new (NULL);
   gtk_widget_ref (entry1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "entry1", entry1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (entry1);
   gtk_table_attach (GTK_TABLE (table1), entry1, 1, 2, 0, 1,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (0), 0, 0);

   combo_entry1 = gnome_entry_gtk_entry (GNOME_ENTRY (entry1));
   gtk_widget_ref (combo_entry1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "combo_entry1", combo_entry1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (combo_entry1);
   gtk_entry_set_text (GTK_ENTRY (combo_entry1), _("buildFunct"));

   entry2 = gnome_entry_new (NULL);
   gtk_widget_ref (entry2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "entry2", entry2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (entry2);
   gtk_table_attach (GTK_TABLE (table1), entry2, 1, 2, 1, 2,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (0), 0, 0);

   combo_entry2 = gnome_entry_gtk_entry (GNOME_ENTRY (entry2));
   gtk_widget_ref (combo_entry2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "combo_entry2", combo_entry2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (combo_entry2);
   gtk_entry_set_text (GTK_ENTRY (combo_entry2), _("build_doc.cc"));

   fileentry1 = gnome_file_entry_new (NULL, NULL);
   gtk_widget_ref (fileentry1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "fileentry1", fileentry1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (fileentry1);
   gtk_table_attach (GTK_TABLE (table1), fileentry1, 1, 2, 2, 3,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (0), 0, 0);

   combo_entry3 = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fileentry1));
   gtk_widget_ref (combo_entry3);
   gtk_object_set_data_full (GTK_OBJECT (window1), "combo_entry3", combo_entry3,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (combo_entry3);

   checkbutton1 = gtk_check_button_new_with_label (_("Generate main"));
   gtk_widget_ref (checkbutton1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "checkbutton1", checkbutton1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (checkbutton1);
   gtk_box_pack_start (GTK_BOX (vbox1), checkbutton1, FALSE, FALSE, 0);

   checkbutton2 = gtk_check_button_new_with_label (_("Static linkage (copy dependencies)"));
   gtk_widget_ref (checkbutton2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "checkbutton2", checkbutton2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (checkbutton2);
   gtk_box_pack_start (GTK_BOX (vbox1), checkbutton2, FALSE, FALSE, 0);

   checkbutton3 = gtk_check_button_new_with_label (_("Generate makefile"));
   gtk_widget_ref (checkbutton3);
   gtk_object_set_data_full (GTK_OBJECT (window1), "checkbutton3", checkbutton3,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (checkbutton3);
   gtk_box_pack_start (GTK_BOX (vbox1), checkbutton3, FALSE, FALSE, 0);

   hseparator2 = gtk_hseparator_new ();
   gtk_widget_ref (hseparator2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "hseparator2", hseparator2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (hseparator2);
   gtk_box_pack_start (GTK_BOX (vbox1), hseparator2, TRUE, TRUE, 0);

   hbuttonbox1 = gtk_hbutton_box_new ();
   gtk_widget_ref (hbuttonbox1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "hbuttonbox1", hbuttonbox1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (hbuttonbox1);
   gtk_box_pack_start (GTK_BOX (vbox1), hbuttonbox1, TRUE, TRUE, 0);
   gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox1), GTK_BUTTONBOX_END);

   button1 = gtk_button_new_from_stock ("gtk-ok");
   gtk_widget_ref (button1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button1", button1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button1);
   gtk_container_add (GTK_CONTAINER (hbuttonbox1), button1);
   GTK_WIDGET_SET_FLAGS (button1, GTK_CAN_DEFAULT);

   gtk_signal_connect (GTK_OBJECT (button1), "clicked",
		       GTK_SIGNAL_FUNC( codegen_ok), this);


   button2 = gtk_button_new_from_stock ("gtk-cancel");
   gtk_widget_ref (button2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button2", button2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button2);
   gtk_container_add (GTK_CONTAINER (hbuttonbox1), button2);
   GTK_WIDGET_SET_FLAGS (button2, GTK_CAN_DEFAULT);

   gtk_signal_connect (GTK_OBJECT (button2), "clicked",
		       GTK_SIGNAL_FUNC( codegen_cancel), this);

   dialog=window1;

   gtk_widget_show(dialog);

}

