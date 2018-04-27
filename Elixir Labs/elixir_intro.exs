defmodule Elixir_Intro do
	def fib(n) do
		case n do
			0->
				0
			1->
				1
			_->
				fibPart(1, 1, n-2)
		end
	end
	def fibPart(a,b,n) do
		case n do
			0 ->
				b
			_ ->
				fibPart(b, a+b, n-1)
		end
	end
	def area(shape, shape_info) do
		case shape do
			:triangle ->
				elem(shape_info, 0) * elem(shape_info, 1) * 0.5
			:rectangle ->
				elem(shape_info, 0) * elem(shape_info, 1)
			:circle ->
				shape_info * shape_info * :math.pi
		end
	end
	def sqrList(nums) do
		map(fn(n) -> n*n end, nums)
	end
	def calcTotals(inventory) do
		map(&Elixir_Intro.calcItemTotal/1, inventory)
	end
	def calcItemTotal(invItem) do
		{elem(invItem, 0), elem(invItem, 1) * elem(invItem, 1)}
	end
	def map(function, vals) do
		case length(vals) do
			0->
				[]
			_->
				[function.(hd(vals))]++map(function, tl(vals))
		end
	end
	def quickSortServer() do
		receive do
			{message, pid}->
				send(pid, {qsort(message), self()})
		end
		quickSortServer()
	end
	def qsort([]) do
		[]
	end
	def qsort(list) do
		pivPos = :rand.uniform(1)-1
		pivot = elem(Enum.fetch(list, pivPos), 1)
		rest = List.delete_at(list, pivPos)
		{left, right} = Enum.split_with(rest, &(&1 < pivot))
		qsort(left) ++ [pivot] ++ qsort(right)
	end
end

defmodule Client do
	def callServer(pid, nums) do
		send(pid, {nums, self()})
		listen()
	end
	def listen do
		receive do
			{sorted, pid}->
				sorted
		end
	end
end