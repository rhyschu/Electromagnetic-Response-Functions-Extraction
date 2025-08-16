import PyPDF2

def merge_pdfs(pdf_list, output_file):
    merger = PyPDF2.PdfMerger()
    
    for pdf in pdf_list:
        merger.append(pdf)
    
    merger.write(output_file)
    merger.close()
    print(f"Merged {len(pdf_list)} PDFs into {output_file}")

pdf_files = ["Fe56_RLRT_1.pdf", "Fe56_RLRT_2.pdf", "Fe56_RLRT_3.pdf"]
output = "Fe56_comparison_Aug16.pdf"
merge_pdfs(pdf_files, output)
