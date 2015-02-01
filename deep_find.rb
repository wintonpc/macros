class DeepFind1
  class FoundIt < StandardError
    attr_accessor :value
    def initialize(value)
      @value = value
    end
  end

  class << self
    def deep_find(x, &pred)
      begin
        deep_find_impl(x, &pred)
        nil
      rescue FoundIt => e
        e.value
      end
    end

    private

    def deep_find_impl(x, &pred)
      puts "looking at #{x}"
      if !x.is_a?(Array)
        if pred.call(x)
          raise FoundIt.new(x)
        end
      else
        x.each{|item| deep_find_impl(item, &pred)}
      end
    end
  end
end

require 'continuation'

class DeepFind2
  class << self
    def deep_find(x, &pred)
      callcc do |k|
        find = proc do |x|
          puts "looking at #{x}"
          if !x.is_a?(Array)
            if pred.call(x)
              k.call(x)
            end
          else
            x.each{|item| find.call(item)}
          end
        end
        find.call(x)
        nil
      end
    end
  end
end

$input = [2,4,[6,7,8],10]
