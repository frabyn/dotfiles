vim.api.nvim_set_keymap(
"n",
"<leader>ff",
":Telescope find_files<cr>",
{ noremap = true }
)

vim.api.nvim_set_keymap(
"n",
"<leader>fg",
":Telescope live_grep<cr>",
{ noremap = true }
)


vim.api.nvim_set_keymap(
"n",
"<leader>fb",
":Telescope buffers<cr>",
{ noremap = true }
)

vim.api.nvim_set_keymap(
"n",
"<leader>fh",
":Telescope help_tags<cr>",
{ noremap = true }
)


local custom_attach = function(client, bufnr)
	-- Mappings.
	local map = function(mode, l, r, opts)
		opts = opts or {}
		opts.silent = true
		opts.buffer = bufnr
		keymap.set(mode, l, r, opts)
	end

	map("n", "gd", vim.lsp.buf.definition, { desc = "go to definition" })
	map("n", "<C-]>", vim.lsp.buf.definition)
	map("n", "K", vim.lsp.buf.hover)
	map("n", "<C-k>", vim.lsp.buf.signature_help)
	map("n", "<space>rn", vim.lsp.buf.rename, { desc = "varialbe rename" })
	map("n", "gr", vim.lsp.buf.references, { desc = "show references" })
	map("n", "[d", diagnostic.goto_prev, { desc = "previous diagnostic" })
	map("n", "]d", diagnostic.goto_next, { desc = "next diagnostic" })
	map("n", "<space>q", diagnostic.setqflist, { desc = "put diagnostic to qf" })
	map("n", "<space>ca", vim.lsp.buf.code_action, { desc = "LSP code action" })
	map("n", "<space>wa", vim.lsp.buf.add_workspace_folder, { desc = "add workspace folder" })
	map("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, { desc = "remove workspace folder" })
	map("n", "<space>wl", function()
		inspect(vim.lsp.buf.list_workspace_folders())
	end, { desc = "list workspace folder" })

	-- Set some key bindings conditional on server capabilities
	if client.server_capabilities.documentFormattingProvider then
		map("n", "<space>f", vim.lsp.buf.format, { desc = "format code" })
	end
end
